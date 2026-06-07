#' Package options
#'
#' These options are best set via [ir_options()] and queried via [ir_get_option()].
#' However, the base functions [options()] and [getOption()] work as well but require
#' an `isoreader2.` prefix (the package name and a dot) for the option name. Setting
#' an option to a value of `NULL` means that the default is used. [ir_get_options()]
#' is available as an additional convenience function to retrieve a subset of options
#' with a regular expression pattern.
#'
#' @examples
#' # All default options
#' ir_get_options()
#'
#' @param ... set package options, syntax identical to [options()]
#' @describeIn ir_options set/get option values
#' @export
ir_options <- function(...) {
  pkg_options(pkg = "isoreader2", pkg_options = get_pkg_options(), ...)
}

#' @param pattern to retrieve multiple options (as a list) with a shared pattern
#' @describeIn ir_options get a subset of option values that fit a pattern
#' @export
ir_get_options <- function(pattern = NULL) {
  pkg_options <- ir_options()
  if (!is.null(pattern)) {
    pkg_options <- pkg_options[grepl(pattern, names(pkg_options))]
  }
  return(pkg_options)
}

#' @describeIn ir_options retrieve the current value of one option (option must be defined for the package)
#' @param x name of the specific option to retrieve
#' @export
ir_get_option <- function(x) {
  get_pkg_option(
    option = x,
    pkg = "isoreader2",
    pkg_options = get_pkg_options()
  )
}

#' @rdname ir_options
#' @format NULL
#' @usage NULL
#' @section Options for the isoreader2 package:
get_pkg_options <- function() {
  list(
    #' - `aggregators`: data aggregators for pulling data out of raw files. The list of available aggregators is accessible via `ir_get_option("aggregators")`. Individiual aggregators are available via the shortcut helper function `ir_get_aggregator("standard")`. Register new/overwrite existing aggregators via `ir_register_aggregator()`.
    aggregators = define_pkg_option(
      default = list(), # default aggregators are registered in zzz.R
      check_fn = function(x) {
        if (
          missing(x) ||
            !is.list(x) ||
            !all(purrr::map_lgl(x, is, "ir_aggregator"))
        ) {
          cli_abort(
            "{.var aggregators} is not a list of {cli::col_magenta('ir_aggregator')} objects. Please use {.strong ir_register_aggregator()} too add individual aggregators."
          )
        }
        return(TRUE)
      }
    ),
    #' - `debug`: turn on debug mode
    debug = define_pkg_option(default = FALSE, check_fn = is_scalar_logical),
    #' - `auto_use_ansi`: whether to automatically enable correct rendering of stylized (ansi) output in HTML reports from notebooks that call `library(isoir)`. Can be turned off by calling `isoir::ir_options(auto_use_ansi = FALSE)` **before** call `library(isoir)`.
    auto_use_ansi = define_pkg_option(
      default = TRUE,
      check_fn = is_scalar_logical
    )
  )
}
