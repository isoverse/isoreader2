# .iarc / .larc — Elementar IonOS/LyticOS archive
read_iarc_json <- function(json_path) {
  # metadata
  metadata <- json_path |>
    read_liarc_metadata() |>
    try_catch_cnds()

  # species
  species <- json_path |>
    read_liarc_species() |>
    try_catch_cnds()

  # method species
  method_species <- json_path |>
    read_liarc_method_species() |>
    try_catch_cnds()

  # traces
  traces <- json_path |>
    read_liarc_traces(
      global_species = species$result,
      method_species = method_species$result
    ) |>
    try_catch_cnds()

  # resistors
  resistors <- json_path |>
    read_liarc_resistors(traces = traces$result) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    metadata$conditions,
    species$conditions,
    method_species$conditions,
    traces$conditions,
    resistors$conditions
  )

  # return value
  tibble(
    metadata = list(metadata$result),
    resistors = list(resistors$result),
    traces = list(traces$result),
    problems = list(problems)
  )
}

# reader is the same
read_larc_json <- read_iarc_json

# Parses ISO 8601 datetime with timezone offset (e.g. "+01:00") to POSIXct UTC.
# %z in strptime requires the compact "+HHMM" form, so the colon must be stripped.
parse_liarc_datetime <- function(x) {
  x <- sub("\\.[0-9]+([+-])", "\\1", x)
  x <- sub("([+-][0-9]{2}):([0-9]{2})$", "\\1\\2", x)
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
}

# Reads h3_correction_value from systems/species across all systems.
# Drops zero and missing values; returns NA_real_ if none remain.
# Warns when multiple distinct values are found and uses the first.
read_liarc_h3_factor <- function(json_path) {
  systems <- query_json(
    json_path,
    "/systems",
    list_as_tibble = TRUE,
    required = FALSE
  )
  if (
    is.null(systems) ||
      json_missing(systems) ||
      nrow(systems) == 0 ||
      !"species" %in% names(systems)
  ) {
    return(NA_real_)
  }
  all_species <- systems |>
    dplyr::select("system_id" = "id", "species") |>
    tidyr::unnest("species")
  if (!"h3_correction_value" %in% names(all_species)) {
    return(NA_real_)
  }
  vals <- all_species |>
    dplyr::filter(
      !is.na(.data$h3_correction_value),
      .data$h3_correction_value != 0
    ) |>
    dplyr::pull("h3_correction_value") |>
    as.numeric() |>
    unique()
  if (length(vals) == 0) {
    return(NA_real_)
  }
  if (length(vals) > 1) {
    cli_warn(
      "multiple h3 factor values found ({.val {vals}}), only the first will be used"
    )
  }
  vals[1]
}

# Reads per-task sequence metadata from /tasks. sample_type and system_description
# are included when present. Key/value pairs from tasks/values are widened between
# Method and the timing columns.
read_liarc_metadata <- function(json_path) {
  # pull out tasks
  tasks <- query_json(json_path, "/tasks", list_as_tibble = TRUE)
  if (is_empty(tasks) || nrow(tasks) == 0) {
    cli::cli_abort("no analyses (tasks) found")
  }

  # processing list name lookup (guid → Sequence)
  pl <- query_json(json_path, "/processing_lists", list_as_tibble = TRUE)

  # method name lookup (id → Method)
  methods <- query_json(json_path, "/methods", list_as_tibble = TRUE)

  # timestamp
  ts <- query_json(json_path, "/created_date")

  # base: system, sequence, id, name, sample type, method
  result <- tibble(
    analysis = seq_len(nrow(tasks)),
    type = "cf",
    timestamp = parse_liarc_datetime(ts),
    h3_factor = read_liarc_h3_factor(json_path),
    System = (tasks[["system_description"]] %||% NA) |> as.character(),
    processing_list_guid = (tasks[["processing_list_guid"]] %||% NA) |>
      as.character()
  )

  # is there processing list info?
  if (!is_empty(pl) && nrow(pl) > 0) {
    result <- result |>
      dplyr::left_join(
        pl |>
          dplyr::select("processing_list_guid" = "guid", "Sequence" = "name"),
        by = "processing_list_guid"
      )
  }

  result <- result |>
    dplyr::mutate(
      Id = (tasks[["id"]] %||% NA) |> as.integer(),
      Name = (tasks[["name"]] %||% NA) |> as.character(),
      `Sample Type` = (tasks[["sample_type"]] %||% NA) |> as.character(),
      method_id = (tasks[["method_id"]] %||% NA) |> as.integer()
    )

  # is there methods info?
  if (!is_empty(methods) && nrow(methods) > 0) {
    result <- result |>
      dplyr::left_join(
        methods |> dplyr::select("method_id" = "id", "Method" = "name"),
        by = "method_id"
      )
  }

  # values: widened key/value pairs inserted after Method
  if ("values" %in% names(tasks)) {
    values_list <- purrr::map(tasks$values, function(v) {
      if (is.list(v) && length(v) > 0) v else NULL
    })
    all_keys <- unique(unlist(purrr::map(values_list, names)))
    for (key in all_keys) {
      result[[key]] <- purrr::map_chr(
        values_list,
        ~ if (is.null(.x) || !key %in% names(.x)) {
          NA_character_
        } else {
          as.character(.x[[key]])
        }
      )
    }
  }

  # timing and completion at the end
  result |>
    dplyr::select(-dplyr::any_of(c("processing_list_guid", "method_id"))) |>
    dplyr::mutate(
      Start = parse_iso8601_datetime(tasks$acquisition_start),
      End = parse_iso8601_datetime(tasks$acquisition_end),
      Completion = (tasks[["completion_state"]] %||% NA) |> as.character()
    )
}

# Reads species from processing_lists[0]/species. Returns a flat tibble
# (species, channel, mass) with one row per beam assignment.
# beam_masses here are derived from ratio labels — incomplete for some species.
read_liarc_species <- function(json_path) {
  species <- query_json(
    json_path,
    "/processing_lists/0/species",
    list_as_tibble = TRUE
  )
  species |>
    dplyr::select("species" = "name", "beam_masses") |>
    tidyr::unnest("beam_masses") |>
    dplyr::mutate(
      channel = readr::parse_number(.data$beam) |> as.integer(),
      mass = as.character(signif(as.numeric(.data$mass), digits = 3))
    ) |>
    dplyr::select("species", "channel", "mass")
}

# Reads per-method beam-mass assignments from methods[]/beam_masses.
# Only present in V3-nested archives (IRMSAcquisitionDisplaySettings).
# Returns a flat tibble (method_id, species, beam, mass), or NULL if absent.
# Method-level beam_masses supersede processing-list-derived values.
read_liarc_method_species <- function(json_path) {
  methods <- query_json(json_path, "/methods", list_as_tibble = TRUE)
  if (!"beam_masses" %in% names(methods)) {
    return(NULL)
  }
  methods |>
    dplyr::filter(
      !purrr::map_lgl(.data$beam_masses, ~ length(.x) == 1 && is.na(.x))
    ) |>
    dplyr::select("method_id" = "id", "beam_masses") |>
    tidyr::unnest("beam_masses") |>
    tidyr::unnest("beams") |>
    dplyr::mutate(
      channel = readr::parse_number(.data$beam) |> as.integer(),
      mass = as.character(signif(as.numeric(.data$mass), digits = 3))
    ) |>
    dplyr::select("method_id", "species", "channel", "mass")
}


# Reads IRMS beam traces from tasks/datasets. Only datasets with Scan + Beam*
# columns are included. time.s is computed from Scan index scaled by the
# dataset's (end - start) duration.
read_liarc_traces <- function(json_path, global_species, method_species) {
  tasks <- query_json(json_path, "/tasks", list_as_tibble = TRUE)
  traces <- tibble(
    analysis = seq_len(nrow(tasks)),
    method_id = (tasks[["method_id"]] %||% NA) |> as.integer(),
    traces = tasks$datasets |>
      purrr::map(function(ds) {
        if (is_empty(ds$data)) {
          return(NULL)
        }
        list(data = ds$data, start = ds$start, end = ds$end) |>
          purrr::pmap(function(data, start, end) {
            # safety check
            if (is_empty(data) || !is_list(data)) {
              return(NULL)
            }
            beam_cols <- names(data)[startsWith(names(data), "beam")]
            if (length(beam_cols) == 0) {
              return(NULL)
            }

            # figure out run duration
            dur_s <- as.numeric(difftime(
              parse_iso8601_datetime(end),
              parse_iso8601_datetime(start),
              units = "secs"
            ))

            # make tibble
            data[c("scan", beam_cols)] |>
              tibble::as_tibble() |>
              dplyr::mutate(
                species = if (!is_empty(data$species) > 0) {
                  as.character(data$species)
                } else {
                  NA_character_
                },
                time.s = .data$scan / max(.data$scan) * dur_s
              ) |>
              tidyr::pivot_longer(
                cols = dplyr::all_of(beam_cols),
                names_to = "channel",
                values_to = "intensity.A"
              ) |>
              dplyr::mutate(
                channel = readr::parse_number(.data$channel) |> as.integer()
              )
          }) |>
          dplyr::bind_rows()
      })
  ) |>
    tidyr::unnest(.data$traces)

  # safety check
  if (nrow(traces) == 0L) {
    return(NULL)
  }

  # add species
  if (!is.null(method_species)) {
    traces <- traces |>
      dplyr::inner_join(
        method_species,
        by = c("method_id", "species", "channel")
      )
  } else if (!is.null(global_species)) {
    traces <- traces |>
      dplyr::inner_join(global_species, by = c("species", "channel"))
  } else {
    cli_warn(
      "channel to mass mappings unavailable; traces {.field mass} set to NA for all channels"
    )
    traces <- traces |> dplyr::mutate(mass = NA_character_)
  }
  # final selection
  traces |>
    dplyr::select(
      "analysis",
      "species",
      "channel",
      "mass",
      "time.s",
      "intensity.A"
    )
}

# Reads resistors from /systems/beams. Filters out beams with no inuse_R_ohm
# (unused channels). Returns NULL if no systems are present (V2 archives).
read_liarc_resistors <- function(json_path, traces) {
  systems <- query_json(
    json_path,
    "/systems",
    list_as_tibble = TRUE,
    required = FALSE
  )
  if (is.null(systems) || json_missing(systems)) {
    return(NULL)
  }

  # pull out all resistors (stored in every system)
  resistors <- systems |>
    dplyr::select("system_id" = "id", "beams") |>
    tidyr::unnest("beams") |>
    dplyr::filter(!is.na(.data$inuse_R_ohm)) |>
    dplyr::mutate(channel = readr::parse_number(.data$beam) |> as.integer()) |>
    dplyr::select(
      "system_id",
      "channel",
      "nominal.Ohm" = "nominal_R_ohm",
      "resistance.Ohm" = "inuse_R_ohm"
    )

  # check for inconsistencies in the resistors
  multiple_resistances <-
    resistors |>
    dplyr::count(.data$channel, .data$nominal.Ohm, .data$resistance.Ohm) |>
    dplyr::count(.data$channel) |>
    dplyr::filter(.data$n > 1)
  if (nrow(multiple_resistances) > 0) {
    cli_warn(
      "encountered multiple resistance values for collector{?s} {multiple_resistances$channel} - storing the first value only"
    )
  }

  # use only the first system resistors
  resistors <-
    resistors |>
    dplyr::filter(.data$system_id == .data$system_id[1]) |>
    dplyr::select(-"system_id")

  # add gas configuration and trace information
  if (!is.null(traces)) {
    resistors <- traces |>
      dplyr::select("species", "channel", "mass") |>
      dplyr::distinct() |>
      dplyr::left_join(resistors, by = "channel")
  } else {
    cli_warn(
      "traces unavailable; resistor {.field species} and {.field mass} set to NA for all channels"
    )
    resistors <- resistors |>
      dplyr::mutate(species = NA_character_, mass = NA_character_)
  }
  resistors |> dplyr::select("species", "channel", "mass", dplyr::everything())
}
