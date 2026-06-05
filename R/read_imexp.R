# .imexp — Qtegra notebook
read_imexp_json <- function(json_path) {
  # # metadata
  metadata <- json_path |>
    read_imexp_metadata() |>
    try_catch_cnds()

  # traces
  traces <- json_path |>
    read_imexp_traces() |>
    try_catch_cnds()

  # resistors
  resistors <- json_path |>
    read_imexp_resistors() |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    metadata$conditions,
    resistors$conditions,
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
