# individual file readers ==================
# Each function reads the .json from one file type

# .dxf — continuous-flow file (CContiniousFlowBlockData root).
# Gas name may be under CGasConfiguration or CNumericValue/CGasConfiguration.
read_dxf_json <- function(json_path) {
  # gas names
  gas_names <- read_isodat_gas_names(
    json_path,
    # two variants: direct path tried first, CNumericValue-wrapped as fallback
    gas_name_ptr = c(
      "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/p/objects/CGasConfiguration/p/p/v",
      "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/p/objects/CNumericValue/CGasConfiguration/p/p/v"
    ),
    sub_methods_ptr = "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/CMethod"
  ) |>
    try_catch_cnds()

  # resistors
  resistors <- read_isodat_resistors(
    json_path,
    hw_list_ptr = "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
    gas_names = gas_names$result
  ) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    gas_names$conditions,
    resistors$conditions
  )

  # return value
  tibble(
    resistors = list(resistors$result),
    problems = list(problems)
  )
}

# .cf — continuous-flow file (CMethod root, simpler structure than .dxf).
read_cf_json <- function(json_path) {
  # gas names
  gas_names <- read_isodat_gas_names(
    json_path,
    gas_name_ptr = "/CMethod/p/objects/CGasConfiguration/p/p/v",
    sub_methods_ptr = "/CMethod/CMethod"
  ) |>
    try_catch_cnds()

  # resistors
  resistors <- read_isodat_resistors(
    json_path,
    hw_list_ptr = "/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
    gas_names = gas_names$result
  ) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    gas_names$conditions,
    resistors$conditions
  )

  # return value
  tibble(
    resistors = list(resistors$result),
    problems = list(problems)
  )
}

# .did — dual-inlet file (CDualInletBlockData root).
# HW info store may be directly under CMethod or wrapped in CNumericValue.
read_did_json <- function(json_path) {
  # gas names
  gas_names <- read_isodat_gas_names(
    json_path,
    gas_name_ptr = "/CDualInletBlockData/p/objects/CMethod/p/objects/CGasConfiguration/p/p/v",
    sub_methods_ptr = "/CDualInletBlockData/p/objects/CMethod/CMethod"
  ) |>
    try_catch_cnds()

  # resistors
  resistors <- read_isodat_resistors(
    json_path,
    # two variants: direct path tried first, CNumericValue-wrapped as fallback
    hw_list_ptr = c(
      "/CDualInletBlockData/p/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
      "/CDualInletBlockData/p/objects/CMethod/p/objects/CNumericValue/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList"
    ),
    gas_names = gas_names$result
  ) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    gas_names$conditions,
    resistors$conditions
  )

  # return value
  tibble(
    resistors = list(resistors$result),
    problems = list(problems)
  )
}

# .caf — dual-inlet file (CBlockDataContext root).
read_caf_json <- function(json_path) {
  # gas names
  gas_names <- read_isodat_gas_names(
    json_path,
    gas_name_ptr = "/CBlockDataContext/p/objects/CMethod/p/objects/CGasConfiguration/p/p/v",
    sub_methods_ptr = "/CBlockDataContext/p/objects/CMethod/CMethod"
  ) |>
    try_catch_cnds()

  # resistors
  resistors <- read_isodat_resistors(
    json_path,
    hw_list_ptr = "/CBlockDataContext/p/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
    gas_names = gas_names$result
  ) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    gas_names$conditions,
    resistors$conditions
  )

  # return value
  tibble(
    resistors = list(resistors$result),
    problems = list(problems)
  )
}

# .scn — scan file. No gas name or calibrated resistors are stored.
read_scn_json <- function(json_path) {
  tibble(
    problems = list(empty_cnds_tibble())
  )
}

# general isodat readers ===================

# Read gas names from an isodat JSON file.
# Returns a character vector with the top-level gas first, followed by one entry
# per sub-method for multi-gas files (sub_methods_ptr is optional; single-gas
# files that lack sub-methods return just the one top-level name).
#
# @param gas_name_ptr JSON pointer(s) to the top-level CGasConfiguration gas name.
# @param sub_methods_ptr JSON pointer to the CMethod sub-method array (multi-gas files).
# @return character vector: top-level gas name first, then one entry per sub-method.
read_isodat_gas_names <- function(json_path, gas_name_ptr, sub_methods_ptr) {
  top_gas <- query_json(json_path, gas_name_ptr)
  # sub-methods only present in multi-gas files; absent ones return NA_complex_
  subs <- query_json(json_path, sub_methods_ptr, required = FALSE)
  sub_gas_names <- if (is.data.frame(subs)) {
    # multiple sub-methods: one row each
    vapply(subs$p, function(p) p$objects$CGasConfiguration$p$p$v, character(1))
  } else if (is.list(subs) && !is.null(subs$p)) {
    # single sub-method: plain list
    subs$p$objects$CGasConfiguration$p$p$v
  } else {
    character(0)
  }
  c(top_gas, sub_gas_names)
}

# Read calibrated Faraday cup resistors from an isodat JSON file.
# Dispatches to single-gas or multi-gas extraction depending on whether the
# hw_list_ptr resolves to a single list (one gas config) or a data.frame (multiple).
# gas_names must have at least as many entries as resistor sets; extra gas names
# (e.g. sub-methods without their own resistor set) are silently ignored.
#
# @param hw_list_ptr JSON pointer(s) to the CEvalIntegrationUnitHWInfoList node.
# @param gas_names character vector of gas names from read_isodat_gas_names().
# @return A tibble with columns gas, mass, channel, cup, resistor.
read_isodat_resistors <- function(json_path, hw_list_ptr, gas_names) {
  resistors <- query_json(json_path, hw_list_ptr)
  n_resistors <- if (is.data.frame(resistors)) nrow(resistors) else 1L
  if (length(gas_names) < n_resistors) {
    cli_abort(
      "found {n_resistors} {.field resistor set{?s}} but only {length(gas_names)} {.field gas name{?s}} ({.emph {gas_names}})"
    )
  }
  if (is.data.frame(resistors)) {
    # multi-gas: one resistor set per row; slice gas_names to match
    purrr::map2(
      resistors$p,
      gas_names[seq_len(n_resistors)],
      extract_resistor_info
    ) |>
      purrr::list_rbind()
  } else {
    # single resistor set: applies to the primary (first) gas
    extract_resistor_info(resistors$p, gas_names[[1L]])
  }
}

# Extract a tibble of cups from the p (parent) node of a CEvalIntegrationUnitHWInfoList entry.
# Each row is one Faraday cup: gas label, m/z mass, hardware channel, cup position, resistor value.
extract_resistor_info <- function(parent_node, gas) {
  hw <- parent_node$objects$CEvalIntegrationUnitHWInfo
  tibble::tibble(
    gas = gas,
    mass = as.numeric(hw$mass),
    channel = as.integer(hw$channel),
    cup = as.integer(hw$cup),
    resistor = as.numeric(hw$resistor)
  )
}
