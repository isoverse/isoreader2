read_dxf_json <- function(json_path) {
  tibble(
    file_path = gsub("\\.json%", "", json_path),
    problems = list(tibble())
  )
}

read_cf_json <- function(json_path) {
  tibble(
    file_path = gsub("\\.json%", "", json_path),
    problems = list(tibble())
  )
}

read_did_json <- function(json_path) {
  tibble(
    file_path = gsub("\\.json%", "", json_path),
    problems = list(tibble())
  )
}

read_caf_json <- function(json_path) {
  tibble(
    file_path = gsub("\\.json%", "", json_path),
    problems = list(tibble())
  )
}

read_scn_json <- function(json_path) {
  tibble(
    file_path = gsub("\\.json%", "", json_path),
    problems = list(tibble())
  )
}

# JSON pointers to CEvalIntegrationUnitHWInfoList, by file type.
# The list is a single object (single-gas) or an array of objects (multi-gas);
# the R code below handles both via is.data.frame() dispatch.
# .did has two variants depending on isodat version; see read_isodat_resistors().
.hw_list_ptr <- list(
  dxf = "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
  cf = "/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
  did_numeric = "/CDualInletBlockData/p/objects/CMethod/p/objects/CNumericValue/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
  did_direct = "/CDualInletBlockData/p/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
  caf = "/CBlockDataContext/p/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList"
)

# JSON pointers to the top-level gas name (CGasConfiguration.p.p.v), by file type.
.gas_name_ptr <- list(
  dxf = "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/p/objects/CGasConfiguration/p/p/v",
  cf = "/CMethod/p/objects/CGasConfiguration/p/p/v",
  did = "/CDualInletBlockData/p/objects/CMethod/p/objects/CGasConfiguration/p/p/v",
  caf = "/CBlockDataContext/p/objects/CMethod/p/objects/CGasConfiguration/p/p/v"
)

# JSON pointers to the sub-method array (CMethod/CMethod), by file type.
# Each sub-method carries its own gas configuration for multi-gas measurements.
.sub_methods_ptr <- list(
  dxf = "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/CMethod",
  cf = "/CMethod/CMethod",
  did = "/CDualInletBlockData/p/objects/CMethod/CMethod",
  caf = "/CBlockDataContext/p/objects/CMethod/CMethod"
)

# Return an ordered character vector of gas names matching the CEvalIntegrationUnitHWInfoList
# entries: top-level gas first, then one entry per sub-method (for multi-gas files).
.get_gas_names <- function(json_path, ext, hw_list) {
  # did always uses the same gas-name path regardless of numeric/direct variant
  gas_ext <- if (ext == "did") "did" else ext
  top_gas <- RcppSimdJson::fload(json_path, query = .gas_name_ptr[[gas_ext]])

  if (!is.data.frame(hw_list)) {
    return(top_gas)
  } # single-gas

  # multi-gas: append gas name from each sub-method in order
  subs <- RcppSimdJson::fload(
    json_path,
    query = .sub_methods_ptr[[gas_ext]],
    query_error_ok = TRUE
  )
  sub_gas_names <- if (is.data.frame(subs)) {
    # multiple sub-methods simplified to a data.frame
    vapply(subs$p, function(p) p$objects$CGasConfiguration$p$p$v, character(1))
  } else if (is.list(subs) && !is.null(subs$p)) {
    # single sub-method returned as a named list
    subs$p$objects$CGasConfiguration$p$p$v
  } else {
    character(0)
  }
  c(top_gas, sub_gas_names)
}

# Extract a tibble of cups from the p (parent) node of a CEvalIntegrationUnitHWInfoList entry.
# Accepts the p sub-object (which has $objects$CEvalIntegrationUnitHWInfo) and the gas name.
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

# Read calibrated Faraday cup resistors from an isodat JSON file produced by isoextract.
#
# Each row corresponds to one cup in one gas configuration and carries the gas
# name, gain-calibrated resistance (ohms), mass (m/z), integration-unit channel
# index, and physical cup number.  Files that measure multiple gases (e.g. CN or
# CNS) produce one group of rows per gas configuration.  The calibrated values
# reflect whichever resistor bank was active at measurement time.
#
# .scn files do not store calibrated resistors; returns NULL for those.
#
# @param json_path Path to the isodat JSON file (e.g. "run.dxf.json").
# @return A tibble with columns gas (character), mass (numeric), channel
#   (integer), cup (integer), resistor (numeric, ohms), or NULL for .scn files.
read_isodat_resistors <- function(json_path) {
  ext <- tolower(tools::file_ext(sub(
    "\\.json$",
    "",
    json_path,
    ignore.case = TRUE
  )))

  if (ext == "scn") {
    return(NULL)
  }

  hw_list <- switch(
    ext,
    dxf = RcppSimdJson::fload(json_path, query = .hw_list_ptr$dxf),
    cf = RcppSimdJson::fload(json_path, query = .hw_list_ptr$cf),
    caf = RcppSimdJson::fload(json_path, query = .hw_list_ptr$caf),
    did = {
      x <- RcppSimdJson::fload(
        json_path,
        query = .hw_list_ptr$did_numeric,
        query_error_ok = TRUE
      )
      if (is.null(x)) {
        RcppSimdJson::fload(json_path, query = .hw_list_ptr$did_direct)
      } else {
        x
      }
    },
    cli::cli_abort("Unsupported file type: .{ext}")
  )

  gas_names <- .get_gas_names(json_path, ext, hw_list)

  if (is.data.frame(hw_list)) {
    # multi-gas: hw_list is a data.frame with p (parent) as a list-column, one row per gas config
    purrr::map2(hw_list$p, gas_names, .extract_hw_info) |> purrr::list_rbind()
  } else {
    # single-gas: hw_list is a named list; pass its p (parent) sub-object
    .extract_hw_info(hw_list$p, gas_names)
  }
}
