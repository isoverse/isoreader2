# on package load
.onAttach <- function(libname, pkgname) {
  # register aggregators
  agg_metadata <-
    ir_start_aggregator("metadata") |>
    # METADATA section ==================
    ir_add_to_aggregator(
      column = "file_name",
      func = "sub",
      args = list(pattern = "\\.[^.]+$", replacement = "")
    ) |>
    ir_add_to_aggregator(column = "analysis", cast = "as.integer") |>
    ir_add_to_aggregator(column = "timestamp", cast = "as.POSIXct")

  # add all the other metadata columns (but leave the minimal with just the first columns)
  agg_metadata |>
    ir_add_to_aggregator(column = "\\1", source = "(.*)", regexp = TRUE) |>
    ir_register_aggregator("metadata")

  # minimal aggregator
  agg_minimal <-
    agg_metadata |>
    # TRACES section ====================
    ir_add_to_aggregator("traces", "analysis", cast = "as.integer") |>
    ir_add_to_aggregator("traces", "species") |>
    ir_add_to_aggregator("traces", "mass") |>
    ir_add_to_aggregator("traces", "tp", cast = "as.integer") |>
    ir_add_to_aggregator("traces", "time.s", cast = "as.numeric") |>
    ir_add_to_aggregator(
      "traces",
      "\\1",
      source = "(intensity\\..*)",
      regexp = TRUE,
      cast = "as.numeric"
    ) |>
    # CYCLES section ====================
    ir_add_to_aggregator("cycles", "analysis", cast = "as.integer") |>
    ir_add_to_aggregator("cycles", "species") |>
    ir_add_to_aggregator("cycles", "cycle", cast = "as.integer") |>
    ir_add_to_aggregator("cycles", "type") |>
    ir_add_to_aggregator("cycles", "mass") |>
    ir_add_to_aggregator(
      "cycles",
      "\\1",
      source = "(intensity\\..*)",
      regexp = TRUE,
      cast = "as.numeric"
    ) |>
    # SCANS section ====================
    ir_add_to_aggregator("scans", "analysis", cast = "as.integer") |>
    ir_add_to_aggregator("scans", "species") |>
    ir_add_to_aggregator("scans", "mass") |>
    ir_add_to_aggregator("scans", "x", cast = "as.numeric") |>
    ir_add_to_aggregator(
      "scans",
      "\\1",
      source = "(intensity\\..*)",
      regexp = TRUE,
      cast = "as.numeric"
    ) |>
    ir_register_aggregator("minimal")

  # standard aggregator
  agg_standard <- agg_minimal |>
    ir_add_to_aggregator(column = "\\1", source = "(.*)", regexp = TRUE) |>
    ir_register_aggregator("standard")

  # extended aggregator
  agg_extended <- agg_standard |>
    ir_add_to_aggregator("traces", "channel", cast = "as.integer") |>
    ir_add_to_aggregator("traces", "config", cast = "as.integer") |>
    ir_add_to_aggregator("cycles", "channel", cast = "as.integer") |>
    ir_add_to_aggregator("scans", "channel", cast = "as.integer") |>
    # RESISTORS section ====================
    ir_add_to_aggregator("resistors", "species") |>
    ir_add_to_aggregator("resistors", "config", cast = "as.integer") |>
    ir_add_to_aggregator("resistors", "channel", cast = "as.integer") |>
    ir_add_to_aggregator("resistors", "mass") |>
    ir_add_to_aggregator("resistors", "cup", cast = "as.integer") |>
    ir_add_to_aggregator("resistors", "resistance.Ohm", cast = "as.numeric") |>
    ir_add_to_aggregator("resistors", "nominal.Ohm", cast = "as.numeric") |>
    # VENDOR DATA TABLE section ====================
    # preserve all (dynamically named) columns; keep the rare string/integer
    # columns as-is and cast everything else to numeric (see keep_as_is())
    ir_add_to_aggregator(
      "vendor_data_table",
      "\\1",
      source = "(.*)",
      regexp = TRUE,
      cast = "keep_as_is"
    ) |>
    ir_register_aggregator("extended")

  # if we're knitting, enable full ansi output (turn off with ir_options(auto_use_ansi = FALSE))
  if (
    ir_get_option("auto_use_ansi") &&
      requireNamespace("knitr", quietly = TRUE) &&
      requireNamespace("fansi", quietly = TRUE)
  ) {
    # are we in the process of knitting html?
    if (knitr::is_html_output()) {
      options(cli.num_colors = 256)
      utils::capture.output(fansi::set_knit_hooks(
        knitr::knit_hooks,
        which = c('output', 'warning', 'error', 'message')
      ))
    }
  }
}
