# .bch — Sercon batch. Orchestrates all sub-readers and collects conditions.
read_bch_json <- function(json_path) {
  # metadata
  metadata <- json_path |>
    read_bch_metadata() |>
    try_catch_cnds()

  # resistors
  resistors <- json_path |>
    read_bch_resistors() |>
    try_catch_cnds()

  # timings
  timings <- json_path |>
    read_bch_timings() |>
    try_catch_cnds()

  # traces
  traces <- json_path |>
    read_bch_traces() |>
    map_bch_traces(
      metadata = metadata$result,
      timings = timings$result,
      resistors = resistors$result
    ) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    metadata$conditions,
    resistors$conditions,
    timings$conditions,
    traces$conditions
  )

  # return value
  tibble(
    metadata = list(metadata$result),
    resistors = list(resistors$result),
    traces = list(traces$result),
    problems = list(problems)
  )
}

# analysis types table
.bch_analysis_types <- tibble(
  Type = c("S", "R", "D", "B", "M"),
  `Type Name` = c("Sample", "Reference", "Dummy", "Blank", "Manual")
)

# Reads per-block sequence metadata from /data/blocks.
read_bch_metadata <- function(json_path) {
  ts <- query_json(json_path, "/header/timestamp")
  blocks <- query_json(json_path, "/data/blocks")
  tibble::tibble(
    analysis = seq_len(nrow(blocks)),
    timestamp = as.POSIXct(ts, format = "%H:%M:%S\t%m-%d-%Y", tz = "UTC"),
    Number = analysis,
    `Sample Name` = as.character(blocks$name),
    Type = as.character(blocks$type),
    Weight = as.numeric(blocks$weight),
    Method = as.character(blocks$method)
  ) |>
    dplyr::left_join(.bch_analysis_types, by = "Type") |>
    dplyr::relocate("Type Name", .after = "Type")
}

# gas channel/mass mapping for sercon systems
.bch_gas_channel_masses <-
  list(
    "H2" = tibble(channel = 1:2, mass = c(2, 3)),
    "N2" = tibble(channel = 1:2, mass = c(28, 29)),
    "CO" = tibble(channel = 1:3, mass = c(28, 29, 30)),
    "NO" = tibble(channel = 1:3, mass = c(30, 31, 32)),
    "O2" = tibble(channel = 1:2, mass = c(32, 34)),
    "CO2" = tibble(channel = 1:3, mass = c(44, 45, 46)),
    "N2O" = tibble(channel = 1:3, mass = c(44, 45, 46)),
    "SO" = tibble(channel = 1:2, mass = c(48, 50)),
    "SO2" = tibble(channel = 1:2, mass = c(64, 66))
  ) |>
  purrr::imap(~ .x |> dplyr::mutate(species = .y)) |>
  purrr::list_rbind() |>
  dplyr::relocate("species", .before = 1L) |>
  dplyr::mutate(mass = as.character(signif(as.numeric(.data$mass), digits = 3)))

# Reads active gas species from /timings peaks and collector resistances from
# /collectors/beams, joined via .bch_gas_channel_masses.
read_bch_resistors <- function(json_path) {
  gases <- query_json(json_path, "/timings", list_as_tibble = TRUE) |>
    tidyr::unnest(peaks) |>
    dplyr::select("gas_species") |>
    dplyr::pull("gas_species") |>
    unique()
  resistors <- query_json(
    json_path,
    "/collectors/beams",
    list_as_tibble = TRUE
  ) |>
    dplyr::select(
      "channel" = "beam_num",
      "resistance.Ohm" = "active_resistance_ohm"
    )
  tibble(species = gases) |>
    dplyr::left_join(.bch_gas_channel_masses, by = "species") |>
    dplyr::left_join(resistors, by = "channel")
}

# Reads /methods and /timings. Joins each method's analysis_timing_file to its
# timing's active peaks.
read_bch_timings <- function(json_path) {
  methods <- query_json(json_path, "/methods", list_as_tibble = TRUE) |>
    dplyr::mutate(
      timing = tools::file_path_sans_ext(.data$analysis_timing_file)
    ) |>
    dplyr::select("method", "timing")
  timings <- query_json(json_path, "/timings", list_as_tibble = TRUE) |>
    tidyr::unnest(peaks) |>
    dplyr::filter(.data$active) |>
    dplyr::select("timing", "species" = "gas_species", "at_time_s")
  methods |> dplyr::left_join(timings, by = "timing") |> dplyr::distinct()
}

# Reads /data/blocks traces. species and mass are NA until map_bch_traces().
read_bch_traces <- function(json_path) {
  blocks <- query_json(json_path, "/data/blocks")
  tibble::tibble(
    analysis = seq_len(nrow(blocks)),
    traces = purrr::map(
      blocks$traces,
      ~ tibble::as_tibble(.x) |>
        dplyr::select("time_s", dplyr::starts_with("beam")) |>
        tidyr::pivot_longer(
          cols = -"time_s",
          names_to = "channel",
          values_to = "intensity.A"
        ) |>
        dplyr::mutate(
          species = NA_character_, # place holder
          channel = readr::parse_number(.data$channel) |> as.integer(),
          mass = NA_character_ # place holder
        ) |>
        dplyr::select(
          "species",
          "channel",
          "mass",
          "time.s" = "time_s",
          "intensity.A"
        )
    )
  ) |>
    tidyr::unnest(.data$traces)
}

# Assigns species to traces via timing windows (per-method from metadata/timings),
# then fills mass from resistors.
map_bch_traces <- function(traces, metadata, timings, resistors) {
  # safety checks
  if (is.null(metadata)) {
    cli_warn(
      "without sequence information, we don't know which methods to use to map time windows to gas species"
    )
    return(traces)
  }
  if (is.null(timings)) {
    cli_warn(
      "without method timings, we don't know how to map time windows to gas species"
    )
    return(traces)
  }

  windows <- timings |>
    dplyr::mutate(
      .by = c("method", "timing"),
      end_time_s = dplyr::lead(as.double(.data$at_time_s), 1, default = Inf)
    )

  analysis_windows <- metadata |>
    dplyr::select("analysis", "Method") |>
    dplyr::left_join(
      windows,
      by = c("Method" = "method"),
      relationship = "many-to-many"
    ) |>
    dplyr::select("analysis", "species", "at_time_s", "end_time_s")

  # join
  by <- dplyr::join_by(
    "analysis" == "analysis",
    "time.s" >= "at_time_s",
    "time.s" < "end_time_s"
  )
  traces <- traces |>
    dplyr::select(-"species") |>
    dplyr::left_join(analysis_windows, by) |>
    dplyr::select(
      "analysis",
      "species",
      "channel",
      "mass",
      "time.s",
      "intensity.A"
    )

  # warning if no resistors
  if (is.null(resistors)) {
    cli_warn(
      "resistors unavailable; {.field mass} set to NA for all channels"
    )
    return(traces)
  }

  # match resistors
  traces |>
    dplyr::select(-"mass") |>
    dplyr::inner_join(
      resistors |> dplyr::select("species", "channel", "mass"),
      by = c("species", "channel")
    ) |>
    dplyr::relocate("mass", .after = "channel")
}
