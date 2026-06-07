# print.ir_isofiles ========

#' @export
print.ir_isofiles <- function(x, ...) {
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
        0L
      } else {
        purrr::map_int(
          .data$metadata,
          ~ if (!is.data.frame(.x) || all(is.na(.x[["analysis"]]))) {
            0L
          } else {
            nrow(.x)
          }
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
      has_traces = if ("traces" %in% names(x)) {
        !purrr::map_lgl(.data$traces, is.null)
      } else {
        FALSE
      },
      trace_info = (if ("traces" %in% names(x)) .data$traces else list(NULL)) |>
        purrr::map_chr(get_trace_info),
      has_cycles = if ("cycles" %in% names(x)) {
        !purrr::map_lgl(.data$cycles, is.null)
      } else {
        FALSE
      },
      cycle_info = (if ("cycles" %in% names(x)) .data$cycles else list(NULL)) |>
        purrr::map_chr(get_cycle_info),
      has_scans = if ("scans" %in% names(x)) {
        !purrr::map_lgl(.data$scans, is.null)
      } else {
        FALSE
      },
      scans_info = (if ("scans" %in% names(x)) .data$scans else list(NULL)) |>
        purrr::map2_chr(
          (if ("metadata" %in% names(x)) {
            .data$metadata
          } else {
            list(NULL)
          }),
          get_scans_info
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
    dplyr::mutate(all_single = all(.data$n_analyses <= 1)) |>
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
            "{col_cyan(n_analyses)} {qty(n_analyses)}analys{?is/es}"
          )
        },
        if (.data$has_cycles[1] || .data$has_traces[1] || .data$has_scans[1]) {
          " with "
        } else {
          "; no data available; "
        },
        if (.data$has_cycles[1]) {
          .data$cycle_info |> paste0("; ")
        },
        if (.data$has_traces[1]) {
          .data$trace_info |> paste0("; ")
        },
        if (.data$has_scans[1]) {
          .data$scans_info |> paste0("; ")
        },
        .data$metadata_info
      )
    )
  # output
  cli_rule(
    center = "{.strong {nrow(x)} isofile{?s} with {sum(lines$n_analyses)} analys{?is/es} - {?process/combine} with ir_aggregate_isofiles()}"
  )
  lines$label |> cli_bullets_raw() |> cli()
}

#' @exportS3Method knitr::knit_print
knit_print.ir_isofiles <- function(x, ...) {
  print(x, ...)
}

# helper function to summarize information about a set of traces
get_trace_info <- function(traces) {
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
      masses = if (all(is.na(.data$mass))) {
        format_inline("channels {.field {channel}}")
      } else {
        format_inline("masses {.field {mass}}")
      },
      info = format_inline(
        "{col_cyan(numbers_to_text(max(n)))} time points for {col_magenta(species[1])} ({masses})"
      )
    ) |>
    dplyr::pull(.data$info) |>
    paste(collapse = "; ")
}

# helper function to summarize information about a set of cycles
get_cycle_info <- function(cycles) {
  if (!is.data.frame(cycles)) {
    return("no data available")
  }
  cycles |>
    dplyr::summarize(
      .by = "species",
      masses = if (all(is.na(.data$mass))) {
        format_inline("channels {.field {channel}}")
      } else {
        format_inline("masses {.field {mass}}")
      },
      info = format_inline(
        "{col_cyan(max(.data$cycle))} sample/standard cycles for {col_magenta(species[1])} ({masses})"
      )
    ) |>
    dplyr::pull(.data$info) |>
    paste(collapse = "; ")
}

# helper function to summarize information about a set of scans
get_scans_info <- function(scans, metadata = NULL) {
  if (!is.data.frame(scans)) {
    return("no data available")
  }
  scans |>
    dplyr::summarize(
      .by = c("species", "channel", "mass"),
      n = dplyr::n()
    ) |>
    dplyr::summarize(
      .by = "species",
      masses = if (all(is.na(.data$mass))) {
        format_inline("channels {.field {channel}}")
      } else {
        format_inline("masses {.field {mass}}")
      },
      info = format_inline(
        "{col_cyan(numbers_to_text(max(n)))} {metadata$scan_type} steps for {col_magenta(species[1])} ({masses})"
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

# print.ir_aggregated_data ========

#' @export
print.ir_aggregated_data <- function(x, ...) {
  cli_rule(
    center = "{.strong aggregated data from {length(unique(x$metadata$uidx))} isofiles with {nrow(x$metadata)} analys{?is/es} - retrieve with ir_get_data()}"
  )

  # details on columns
  x |>
    purrr::map2_chr(
      names(x),
      function(dataset, name) {
        if (ncol(dataset) == 0) {
          return(NA_character_)
        }
        # summarize problems
        if (name == "problems") {
          return(
            summarize_cnds(
              dataset,
              include_symbol = FALSE,
              include_call = FALSE,
              summary_format = paste0(
                "{symbol$arrow_right} {cli::col_blue('",
                name,
                "')}: ",
                if (nrow(dataset) > 0) {
                  "has a total of {issues} "
                } else {
                  "has {issues}"
                },
                if (nrow(dataset) > 0) {
                  "{symbol$arrow_right} check with {.strong ir_show_problems(x)} / {.strong ir_get_problems(x)}"
                }
              )
            )
          )
        }

        # all other fields
        n_rows <- nrow(dataset) |> numbers_to_text()
        n_na <- purrr::map_int(dataset, ~ sum(is.na(.x)))
        cols <- names(dataset) |>
          sprintf(fmt = "{.field %s}") |>
          paste0(dplyr::case_when(
            n_na == nrow(dataset) ~ " ({col_yellow('all NA')})",
            n_na > 0 ~ sprintf(
              " ({col_yellow('%s NA')})",
              n_na |> numbers_to_text()
            ),
            .default = ""
          ))
        unused_cols <- sprintf(
          "{.emph {col_yellow('%s')}}",
          attr(dataset, "unused_columns")
        )
        format_inline(
          "{symbol$arrow_right} {cli::col_blue(name)} ({n_rows}): ",
          "{glue::glue_collapse(cols, sep = ', ')}",
          if (length(unused_cols) > 0) {
            "; ({.emph not aggregated}: {glue::glue_collapse(unused_cols, sep = ', ')})"
          }
        )
      }
    ) |>
    na.omit() |>
    cli_bullets() # no |> cli() at the to keep paragraphs a bit separated in knitted doc
}

#' @exportS3Method knitr::knit_print
knit_print.ir_aggregated_data <- function(x, ...) {
  print(x, ...)
}

# print.ir_aggregator ========

#' @export
print.ir_aggregator <- function(x, ...) {
  cli_rule(center = "{.strong Aggregator {.emph {attr(x, 'name')}}}")
  if (nrow(x) > 0) {
    summarize_aggregator(x) |>
      dplyr::mutate(
        label = purrr::pmap_chr(
          list(
            .data$column,
            .data$value,
            .data$default,
            .data$regexp,
            .data$source
          ),
          function(col, value, default, regex, source) {
            if (regex) {
              matches <- regmatches(
                source,
                gregexpr("\\([^?)][^()]*\\)", source)
              )[[1]]
              for (i in seq_along(matches)) {
                col <- gsub(sprintf("\\%d", i), matches[[i]], col, fixed = TRUE)
              }
              col <- gsub("\\", "\\\\", col, fixed = TRUE)
              format_inline(
                "{symbol$arrow_right} {cli::col_magenta(col)} = {.emph {value}}"
              )
            } else if (!is.na(default[[1]])) {
              format_inline(
                "{symbol$arrow_right} {.field {col}} = {.emph {value}} - {col_yellow('if source is missing')}: {.field {col}} = {.emph {default[[1]]}}"
              )
            } else {
              format_inline(
                "{symbol$arrow_right} {.field {col}} = {.emph {value}}"
              )
            }
          }
        )
      ) |>
      dplyr::summarise(
        .by = "dataset",
        label = list(c(
          format_inline(
            "{.strong Dataset} {cli::col_blue(.data$dataset[[1]])}:"
          ),
          paste0("\u00a0", .data$label)
        ))
      ) |>
      dplyr::pull(.data$label) |>
      unlist() |>
      cli_bullets_raw() |>
      cli()
  }
}

#' @exportS3Method knitr::knit_print
knit_print.ir_aggregator <- function(x, ...) {
  print(x, ...)
}
