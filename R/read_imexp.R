# .imexp — Qtegra notebook
read_imexp_json <- function(json_path) {
  # metadata
  metadata <- json_path |>
    read_imexp_metadata() |>
    try_catch_cnds()

  # resistors
  resistors <- json_path |>
    read_imexp_resistors() |>
    try_catch_cnds()

  # traces
  traces <- json_path |>
    read_imexp_traces() |>
    map_imexp_traces(
      metadata = metadata$result,
      resistors = resistors$result
    ) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    metadata$conditions,
    resistors$conditions,
    traces$conditions
  )

  # return value (drop guid from metadata now that traces are mapped)
  tibble(
    metadata = list(metadata$result |> dplyr::select(-dplyr::any_of("guid"))),
    resistors = list(resistors$result),
    traces = list(traces$result),
    problems = list(problems)
  )
}

# Reads Faraday cup resistors from settings/amplifiers joined with gas configurations
# via cup_configurations/amplifier_identifier. channel is the 1-based position within
# each gas configuration's cup list; cup is the physical cup number from the display_name.
# config is the 1-based integer index of the settings snapshot in the archive.
# Returns a flat tibble: config, species, channel, cup, mass, resistance.Ohm.
read_imexp_resistors <- function(json_path) {
  settings <- query_json(json_path, "/settings", list_as_tibble = TRUE)
  settings <- settings |>
    dplyr::mutate(config = dplyr::row_number())

  # amplifier lookup: settings_id × amplifier_identifier → resistance.Ohm
  amps <- settings |>
    dplyr::select("settings_id", "amplifiers") |>
    tidyr::unnest("amplifiers") |>
    dplyr::select(
      "settings_id",
      "amplifier_identifier" = "identifier",
      "resistance.Ohm" = "resistor_ohm"
    ) |>
    dplyr::mutate(resistance.Ohm = as.numeric(.data$resistance.Ohm))

  # gas configurations → cups → join amplifier resistances
  settings |>
    dplyr::select("config", "settings_id", "gas_configurations") |>
    tidyr::unnest("gas_configurations") |>
    dplyr::rename("species" = "display_name") |>
    dplyr::select("config", "settings_id", "species", "cup_configurations") |>
    tidyr::unnest("cup_configurations") |>
    dplyr::mutate(
      channel = dplyr::row_number(),
      .by = c("config", "species")
    ) |>
    dplyr::mutate(
      cup = readr::parse_number(.data$display_name) |> as.integer(),
      mass = as.character(signif(as.numeric(.data$mass), digits = 3))
    ) |>
    dplyr::left_join(amps, by = c("settings_id", "amplifier_identifier")) |>
    dplyr::select(
      "config",
      "species",
      "channel",
      "cup",
      "mass",
      "resistance.Ohm"
    )
}

# Reads per-sample metadata from the TFS253Plus sample list joined with config
# (1-based integer index of the settings snapshot used), h3_factor from
# entries/additional_data/linearity_correction (ratio "3H.H/2H.H", factor × conversion),
# and params columns (label → column name, value type-enforced as string/int/double).
# Only rows with a matching entry (i.e. that have been run) are returned.
# Returns a tibble: analysis, timestamp, config, h3_factor, type, guid, Identifier, Comment, <params...>.
read_imexp_metadata <- function(json_path) {
  # settings_id → config integer mapping (ordered as in the archive)
  settings <- query_json(json_path, "/settings", list_as_tibble = TRUE)
  config_map <- tibble::tibble(
    settings_id = as.character(settings$settings_id),
    config = seq_len(nrow(settings))
  )

  # TFS253Plus sample list
  sample_lists <- query_json(json_path, "/sample_lists", list_as_tibble = TRUE)
  tfs_idx <- which(grepl(
    "TFS253Plus\\.samples",
    sample_lists$source,
    ignore.case = TRUE
  ))
  if (length(tfs_idx) == 0) {
    cli_abort("no TFS253Plus.samples found in {.path {json_path}}")
  }
  all_rows <- sample_lists$rows[[tfs_idx[1]]] |> tibble::as_tibble()

  samples <- all_rows |>
    dplyr::select(
      "type" = "sample_line_region",
      "Number" = "run_id",
      "guid",
      "Identifier" = "identifier",
      "Comment" = "comment"
    )

  # params: pivot each row's label/type/value into typed columns
  params_wide <- purrr::map(all_rows$params, function(p) {
    if (is.null(p) || nrow(p) == 0L) {
      return(tibble::tibble(.rows = 1L))
    }
    vals <- purrr::map2(p$value, p$type, function(v, t) {
      if (is.na(v) || identical(v, "")) {
        switch(t, "int" = NA_integer_, "double" = NA_real_, NA_character_)
      } else {
        switch(t, "int" = as.integer(v), "double" = as.double(v), as.character(v))
      }
    })
    names(vals) <- p$label
    tibble::as_tibble(vals)
  }) |>
    dplyr::bind_rows()

  samples <- dplyr::bind_cols(samples, params_wide)

  # entry data: session_time → timestamp, settings_id → config, h3_factor per guid
  entries <- query_json(json_path, "/entries", list_as_tibble = TRUE)
  entry_data <- tibble::tibble(
    guid = as.character(entries$guid),
    timestamp = parse_iso8601_datetime(entries$session_time),
    settings_id = as.character(entries$settings_id),
    h3_factor = purrr::map_dbl(entries$additional_data, function(ad) {
      rr <- ad$linearity_correction$ratio_results
      if (is.null(rr) || nrow(rr) == 0) {
        return(NA_real_)
      }
      h2 <- rr[rr$name == "3H.H/2H.H", ]
      if (nrow(h2) == 0) {
        return(NA_real_)
      }
      as.numeric(h2$linearity_correction_factor[1] * h2$conversion_factor[1])
    })
  ) |>
    dplyr::left_join(config_map, by = "settings_id") |>
    dplyr::select(-"settings_id")

  # inner join: keep only samples that have been run (guid kept for downstream joins)
  samples |>
    dplyr::inner_join(entry_data, by = "guid") |>
    dplyr::select(-"type") |>
    dplyr::mutate(analysis = dplyr::row_number(), .before = 1L) |>
    dplyr::mutate(type = "cf", .after = "analysis") |>
    dplyr::relocate("timestamp", "config", "h3_factor", .after = "type")
}

# Reads raw entry traces. Returns a flat tibble:
# entry_id, settings_id, mass (char), time.s (dbl), intensity.cps (dbl).
read_imexp_traces <- function(json_path) {
  entries <- query_json(json_path, "/entries", list_as_tibble = TRUE)

  tibble::tibble(
    entry_id = as.character(entries$guid),
    settings_id = as.character(entries$settings_id),
    segments = entries$segments
  ) |>
    tidyr::unnest("segments") |>
    dplyr::mutate(
      data = purrr::map2(.data$time_s, .data$channels, function(ts, ch) {
        n_ch <- nrow(ch)
        ch |>
          dplyr::select("mass", "intensity.cps" = "intensity") |>
          dplyr::mutate(
            mass = as.character(signif(as.numeric(.data$mass), digits = 3))
          ) |>
          tidyr::unnest("intensity.cps") |>
          dplyr::mutate(time.s = rep(ts, n_ch))
      })
    ) |>
    dplyr::select("entry_id", "settings_id", "data") |>
    tidyr::unnest("data")
}

# Assigns analysis numbers and species/channel to raw traces.
# metadata must have columns: analysis (int), guid (chr), config (int).
# resistors must have columns: config, species, channel, mass.
map_imexp_traces <- function(traces, metadata, resistors) {
  if (is.null(traces)) {
    return(traces)
  }

  # entry_id → analysis + config via metadata guid
  if (is.null(metadata)) {
    cli_warn(
      "without sample list information, analysis numbers cannot be assigned to traces"
    )
    traces <- traces |>
      dplyr::mutate(analysis = NA_integer_, config = NA_integer_) |>
      dplyr::select(-"entry_id", -"settings_id")
  } else {
    guid_map <- metadata |> dplyr::select("analysis", "config", "guid")
    traces <- traces |>
      dplyr::inner_join(guid_map, by = c("entry_id" = "guid")) |>
      dplyr::select(-"entry_id", -"settings_id")
  }

  # join species/channel from resistors via config + mass
  if (is.null(resistors)) {
    cli_warn(
      "resistors unavailable; {.field species} and {.field channel} set to NA"
    )
    traces <- traces |>
      dplyr::mutate(species = NA_character_, channel = NA_integer_)
  } else {
    traces <- traces |>
      dplyr::left_join(
        resistors |> dplyr::select("config", "species", "channel", "mass"),
        by = c("config", "mass")
      )
  }

  traces |>
    dplyr::select(
      "analysis",
      "config",
      "species",
      "channel",
      "mass",
      "time.s",
      "intensity.cps"
    )
}
