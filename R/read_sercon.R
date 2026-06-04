# .bch — Sercon batch
read_bch_json <- function(json_path) {
  # sequence info
  seq_info <- json_path |>
    read_bch_seq_info() |>
    try_catch_cnds()

  # traces
  traces <- json_path |>
    read_bch_traces() |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    seq_info$conditions,
    traces$conditions
  )

  # return value
  tibble(
    seq_info = list(seq_info$result),
    traces = list(traces$result),
    problems = list(problems)
  )
}

# analysis types table
.bch_analysis_types <- tibble(
  Type = c("S", "R", "D", "B", "M"),
  `Type Name` = c("Sample", "Reference", "Dummy", "Blank", "Manual")
)

# Read per-block sequence metadata from /data/blocks.
# Returns a tibble with one row per block: Sample Name (chr), Type (chr),
# Weight (dbl), Method (chr).
read_bch_seq_info <- function(json_path) {
  blocks <- query_json(json_path, "/data/blocks")
  tibble::tibble(
    "Analysis" = seq_len(nrow(blocks)),
    "Sample Name" = as.character(blocks$name),
    "Type" = as.character(blocks$type),
    "Weight" = as.numeric(blocks$weight),
    "Method" = as.character(blocks$method)
  ) |>
    dplyr::left_join(.bch_analysis_types, by = "Type") |>
    dplyr::relocate("Type Name", .after = "Type")
}

read_bch_traces <- function(json_path) {}
