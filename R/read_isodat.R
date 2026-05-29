read_dxf_json <- function(json_path) {
  problems <- empty_cnds_tibble()
  resistors <- read_isodat_resistors(
    json_path,
    hw_list_ptr = "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
    gas_name_ptr = c(
      "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/p/objects/CNumericValue/CGasConfiguration/p/p/v",
      "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/p/objects/CGasConfiguration/p/p/v"
    ),
    sub_methods_ptr = "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/CMethod"
  ) |>
    try_catch_cnds()
  problems <- dplyr::bind_rows(problems, resistors$conditions)
  tibble(
    resistors = list(resistors$result),
    problems = list(problems)
  )
}

read_cf_json <- function(json_path) {
  problems <- empty_cnds_tibble()
  resistors <- read_isodat_resistors(
    json_path,
    hw_list_ptr = "/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
    gas_name_ptr = "/CMethod/p/objects/CGasConfiguration/p/p/v",
    sub_methods_ptr = "/CMethod/CMethod"
  ) |>
    try_catch_cnds()
  problems <- dplyr::bind_rows(problems, resistors$conditions)
  tibble(
    resistors = list(resistors$result),
    problems = list(problems)
  )
}

read_did_json <- function(json_path) {
  problems <- empty_cnds_tibble()
  resistors <- read_isodat_resistors(
    json_path,
    # two variants depending on isodat version: numeric path tried first, direct path as fallback
    hw_list_ptr = c(
      "/CDualInletBlockData/p/objects/CMethod/p/objects/CNumericValue/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
      "/CDualInletBlockData/p/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList"
    ),
    gas_name_ptr = "/CDualInletBlockData/p/objects/CMethod/p/objects/CGasConfiguration/p/p/v",
    sub_methods_ptr = "/CDualInletBlockData/p/objects/CMethod/CMethod"
  ) |>
    try_catch_cnds()
  problems <- dplyr::bind_rows(problems, resistors$conditions)
  tibble(
    resistors = list(resistors$result),
    problems = list(problems)
  )
}

read_caf_json <- function(json_path) {
  problems <- empty_cnds_tibble()
  resistors <- read_isodat_resistors(
    json_path,
    hw_list_ptr = "/CBlockDataContext/p/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
    gas_name_ptr = "/CBlockDataContext/p/objects/CMethod/p/objects/CGasConfiguration/p/p/v",
    sub_methods_ptr = "/CBlockDataContext/p/objects/CMethod/CMethod"
  ) |>
    try_catch_cnds()
  problems <- dplyr::bind_rows(problems, resistors$conditions)
  tibble(
    resistors = list(resistors$result),
    problems = list(problems)
  )
}

read_scn_json <- function(json_path) {
  # .scn files do not store calibrated resistors
  tibble(
    problems = list(empty_cnds_tibble())
  )
}

# Extract a tibble of cups from the p (parent) node of a CEvalIntegrationUnitHWInfoList entry.
.extract_hw_info <- function(parent_node, gas) {
  hw <- parent_node$objects$CEvalIntegrationUnitHWInfo
  tibble::tibble(
    gas = gas,
    mass = as.numeric(hw$mass),
    channel = as.integer(hw$channel),
    cup = as.integer(hw$cup),
    resistor = as.numeric(hw$resistor)
  )
}

# Return an ordered character vector of gas names matching the hw_list entries:
# top-level gas first, then one entry per sub-method (for multi-gas files).
.get_gas_names <- function(json_path, gas_name_ptr, sub_methods_ptr, hw_list) {
  top_gas <- query_json(json_path, gas_name_ptr)

  if (!is.data.frame(hw_list)) {
    return(top_gas)
  } # single-gas

  # multi-gas: append gas name from each sub-method in order
  subs <- query_json(json_path, sub_methods_ptr, required = FALSE)
  sub_gas_names <- if (is.data.frame(subs)) {
    vapply(subs$p, function(p) p$objects$CGasConfiguration$p$p$v, character(1))
  } else if (is.list(subs) && !is.null(subs$p)) {
    subs$p$objects$CGasConfiguration$p$p$v
  } else {
    character(0)
  }
  c(top_gas, sub_gas_names)
}

# Read calibrated Faraday cup resistors from an isodat JSON file.
#
# @param hw_list_ptr JSON pointer(s) to the CEvalIntegrationUnitHWInfoList node.
#   A character vector of length > 1 is tried in order; the first non-NA_complex_ result
#   is used (e.g. the two .did variants).
# @param gas_name_ptr JSON pointer to the top-level CGasConfiguration gas name.
# @param sub_methods_ptr JSON pointer to the CMethod sub-method array (multi-gas).
# @return A tibble with columns gas, mass, channel, cup, resistor.
read_isodat_resistors <- function(
  json_path,
  hw_list_ptr,
  gas_name_ptr,
  sub_methods_ptr
) {
  hw_list <- query_json(json_path, hw_list_ptr)

  gas_names <- .get_gas_names(json_path, gas_name_ptr, sub_methods_ptr, hw_list)

  if (is.data.frame(hw_list)) {
    # multi-gas: hw_list is a data.frame with p as a list-column, one row per gas config
    purrr::map2(hw_list$p, gas_names, .extract_hw_info) |> purrr::list_rbind()
  } else {
    # single-gas: hw_list is a named list
    .extract_hw_info(hw_list$p, gas_names)
  }
}
