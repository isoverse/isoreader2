# print.isofiles ========

#' @export
print.isofiles <- function(x, ...) {
  n_digits <- function(x) {
    ifelse(x == 0, 1, floor(log10(abs(x))) + 1)
  }

  if (nrow(x) == 0) {
    cli_rule(center = "{.strong 0 isofiles}")
    return()
  }

  lines <- x |>
    dplyr::mutate(
      idx = dplyr::row_number(),
      idx_spacers = max(n_digits(.data$idx)) - n_digits(.data$idx),
      n_analyses = if (!"metadata" %in% names(x)) {
        1L
      } else {
        purrr::map_int(
          .data$metadata,
          ~ if (!is.data.frame(.x)) 1L else nrow(.x)
        )
      },
      filename_spacers = max(nchar(basename(.data$file_path))) -
        nchar(basename(.data$file_path)),
      ext = tools::file_ext(.data$file_path),
      metadata_info = if (!"metadata" %in% names(x)) {
        get_metadata_info(NULL)
      } else {
        purrr::map_chr(.data$metadata, get_metadata_info)
      },
      trace_info = purrr::pmap_chr(
        list(
          ext = .data$ext,
          file_info = if ("file_info" %in% names(x)) {
            .data$file_info
          } else {
            list(NULL)
          },
          traces = if ("traces" %in% names(x)) .data$traces else list(NULL)
        ),
        get_trace_info
      ),
      cycle_info = purrr::pmap_chr(
        list(
          ext = .data$ext,
          file_info = if ("file_info" %in% names(x)) {
            .data$file_info
          } else {
            list(NULL)
          },
          cycles = if ("cycles" %in% names(x)) .data$cycles else list(NULL)
        ),
        get_cycle_info
      ),
      n_problems = purrr::map_int(.data$problems, nrow),
      problems_text = purrr::map_chr(
        .data$problems,
        summarize_cnds,
        include_symbol = FALSE,
        include_call = FALSE
      )
    ) |>
    dplyr::left_join(.file_type_specs, by = c("ext" = "file_type")) |>
    dplyr::mutate(all_single = all(.data$n_analyses == 1)) |>
    dplyr::mutate(
      .by = "idx",
      # format_inline needs a single line
      label = paste0(
        strrep("\u00a0", .data$idx_spacers),
        format_inline("{idx}. {cli::col_blue(basename(file_path))}: "),
        strrep("\u00a0", .data$filename_spacers),
        dplyr::if_else(
          .data$n_problems > 0,
          format_inline("encountered {problems_text}; "),
          ""
        ),
        if (!.data$all_single[1]) {
          format_inline(
            "{col_cyan(n_analyses)} {qty(n_analyses)}analys{?is/es} with "
          )
        },
        if (.data$analysis_type[1] == "dual inlet") {
          .data$cycle_info
        } else {
          .data$trace_info
        },
        "; ",
        .data$metadata_info
      )
    )
  # output
  cli_rule(
    center = "{.strong {nrow(x)} isofile{?s} with {sum(lines$n_analyses)} analys{?is/es} - {?process/combine} with ir_aggregate_isofiles()}"
  )
  lines$label |> cli_bullets_raw() |> cli()
}

#' @export
knit_print.isofiles <- function(x, ...) {
  print(x, ...)
}

# helper function to summarize information about a set of traces
get_trace_info <- function(ext, file_info, traces) {
  if (!is.data.frame(traces)) {
    return("no data available")
  }
  traces |>
    dplyr::summarize(
      .by = c("species", "channel", "mass"),
      n = dplyr::n()
    ) |>
    dplyr::summarize(
      .by = "species",
      masses = if (all(is.na(mass))) {
        format_inline("channels {.field {channel}}")
      } else {
        format_inline("masses {.field {mass}}")
      },
      data_point_type = if (ext == "scn") {
        format_inline("{file_info$type} steps")
      } else {
        "time points"
      },
      info = format_inline(
        "{col_cyan(numbers_to_text(max(n)))} {data_point_type} for {col_magenta(species[1])} ({masses})"
      )
    ) |>
    dplyr::pull(.data$info) |>
    paste(collapse = "; ")
}

# helper function to summarize information about a set of cycles
get_cycle_info <- function(ext, file_info, cycles) {
  if (!is.data.frame(cycles)) {
    return("no data available")
  }
  cycles |>
    dplyr::summarize(
      .by = "species",
      info = format_inline(
        "{col_cyan(max(.data$cycle))} sample/standard cycles for {col_magenta(species[1])} (masses {.field {unique(mass)}})"
      )
    ) |>
    dplyr::pull(.data$info) |>
    paste(collapse = "; ")
}

# helper function to summarize information about the metadata
get_metadata_info <- function(metadata) {
  if (!is.data.frame(metadata)) {
    return("no metadata available")
  }
  format_inline("{col_cyan(ncol(metadata))} metadata columns")
}
