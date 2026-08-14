#' @details
#'
#' Resources:
#'   * Website for the isoreader2 package: <https://isoreader2.isoverse.org>
#'   * Package options: [ir_options]
"_PACKAGE"

## usethis namespace: start
#' @import cli
#' @import rlang
#' @import ggplot2
#' @importFrom tibble tibble is_tibble as_tibble
#' @importFrom methods is
#' @importFrom grDevices palette.colors
#' @importFrom utils tail
#' @importFrom stats setNames
#' @importFrom dplyr everything all_of
## usethis namespace: end
utils::globalVariables(c("data_type", "file_name", "mass", "species", "type"))
NULL

# minimum required isoextract versions for different file types
.file_type_specs <- tibble(
  file_type = c(
    "dxf",
    "cf",
    "iarc",
    "larc",
    "bch",
    "imexp",
    "did",
    "caf",
    "scn"
  ),
  min_isoextract_version = "0.3.0",
  vendor_software = c(
    "Isodat",
    "Isodat",
    "IonOS",
    "LyticOS",
    "Callisto",
    "Qtegra",
    "Isodat",
    "Isodat",
    "Isodat"
  )
)

#' Get supported file types
#' @export
#' @return a tibble of the file types supported by this package
#' @examples
#' ir_get_supported_file_types()
ir_get_supported_file_types <- function() {
  .file_type_specs
}
