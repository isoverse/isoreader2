#' @details
#'
#' Resources:
#'   * Website for the isoreader2 package: <https://isoreader2.isoverse.org>
#'   * Package options: [isor_options]
"_PACKAGE"

## usethis namespace: start
#' @import cli
#' @import rlang
#' @importFrom tibble tibble
## usethis namespace: end
NULL

# minimum required isoextract versions for different file types
.file_type_specs <- tibble::tibble(
  file_type = c("dxf", "cf", "did", "caf", "scn"),
  min_isoextract_version = c("0.1.2", "0.1.2", "0.1.2", "0.1.2", "0.1.2")
)
