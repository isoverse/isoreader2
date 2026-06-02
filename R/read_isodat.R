# individual file readers ==================
# Each function reads the .json from one file type

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
# Each row is one Faraday cup: analyte (gas label), m/z mass, hardware channel, cup position, resistor value.
extract_resistor_info <- function(parent_node, gas) {
  hw <- parent_node$objects$CEvalIntegrationUnitHWInfo
  tibble::tibble(
    analyte = as.factor(gas),
    mass = as.factor(hw$mass),
    channel = as.integer(hw$channel),
    cup = as.integer(hw$cup),
    resistor = as.numeric(hw$resistor)
  )
}

# Read nominal Faraday cup resistors for a .scn file.
# .scn files store no calibrated resistor table; nominal values come from CCupHardwarePart.
# Active cups and their mass/channel assignments are taken from CChannelGasConfPart;
# resistor values are looked up by matching cup position to CCupHardwarePart$idx.
# analyte+ is NA because .scn files carry no gas name.
read_isodat_scn_resistors <- function(json_path, cup_hw_ptr, channel_gas_ptr) {
  hw <- query_json(json_path, cup_hw_ptr)
  channels <- query_json(json_path, channel_gas_ptr)
  tibble::tibble(
    mass = as.factor(channels$mass),
    channel = as.integer(channels$idx),
    cup = as.integer(channels$cup),
    resistor = as.numeric(hw$resistor[match(channels$cup, hw$idx)])
  )
}

# Parse an ISO 8601 datetime string (with optional timezone offset) to POSIXct UTC.
parse_isodat_datetime <- function(x) {
  as.POSIXct(
    sub("[+-]\\d{2}:\\d{2}$", "", x),
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )
}


# file type specific readers ==========

## dxf =====================

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

  # raw trace data from RawDataBlock
  raw_data <- read_isodat_dxf_raw_data(json_path) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    gas_names$conditions,
    resistors$conditions,
    raw_data$conditions
  )

  # return value
  tibble(
    resistors = list(resistors$result),
    raw_data = list(raw_data$result),
    problems = list(problems)
  )
}

# Find the integer index of a named CBlockData block in a .dxf JSON file.
# Scans /CContiniousFlowBlockData/p/objects/CBlockData/N/p/l for N = 0..max_idx.
# Aborts if the label is not found.
find_dxf_block_idx <- function(json_path, label, max_idx = 9L) {
  base <- "/CContiniousFlowBlockData/p/objects/CBlockData"
  for (i in seq(0L, max_idx)) {
    l <- query_json(json_path, paste0(base, "/", i, "/p/l"), required = FALSE)
    if (!json_missing(l) && identical(l, label)) return(i)
  }
  cli_abort(
    "no CBlockData entry with label {.val {label}} found in {.path {json_path}}"
  )
}

# Read raw trace data from the RawDataBlock of a .dxf JSON file.
# Returns a tibble with one row per gas:
#   gas     <chr>  — gas formula (e.g. "H2", "CO")
#   x       <list> — numeric vector of time values (seconds)
#   traces  <list> — matrix n_traces × n_pts; each row is one Faraday cup trace
read_isodat_dxf_raw_data <- function(json_path) {
  raw_idx <- find_dxf_block_idx(json_path, "RawDataBlock")
  raw_data_ptr <- paste0(
    "/CContiniousFlowBlockData/p/objects/CBlockData/",
    raw_idx,
    "/objects/CRawData"
  )
  craw <- query_json(json_path, raw_data_ptr)

  if (is.data.frame(craw)) {
    # multi-gas: CRawData is an array, one row per gas
    purrr::map2(
      craw$complete_formula,
      craw$p,
      function(gas, p) {
        eval_pp <- p$CEvalGCData$p$p
        tibble::tibble(
          gas = gas,
          x = list(eval_pp$x),
          traces = list(eval_pp$traces)
        )
      }
    ) |>
      purrr::list_rbind()
  } else {
    # single gas: CRawData is a plain list
    eval_pp <- craw$p$CEvalGCData$p$p
    tibble::tibble(
      gas = craw$complete_formula,
      x = list(eval_pp$x),
      traces = list(eval_pp$traces)
    )
  }
}

## cf ===============

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

  # raw trace data
  raw_data <- read_isodat_cf_raw_data(json_path, gas_names$result) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    gas_names$conditions,
    resistors$conditions,
    raw_data$conditions
  )

  # return value
  tibble(
    resistors = list(resistors$result),
    raw_data = list(raw_data$result),
    problems = list(problems)
  )
}

# Read raw trace data from a .cf JSON file.
# .cf files store raw data under /CBlockData/0/objects/CBlockDataContext with three variants:
#   A) single gas, CRawData: context -> inner CBlockDataContext -> CBlockData -> CRawData
#      data at CRawData/p/CEvalGCData/p/p (x, traces matrix)
#   B) single gas, CRawDataScanStorage: context -> CBlockData -> CRawDataScanStorage
#      data at CRawDataScanStorage/p/CBinary (x, traces matrix)
#   C) multi-gas, indexed CRawDataScanStorage: context[i] -> CBlockData -> CRawDataScanStorage
#      same CBinary data path, one entry per gas matching gas_names order
# gas_names must be provided (from read_isodat_gas_names); .cf files do not embed formula in the data block.
# Returns a tibble with columns gas (chr), x (list of numeric vectors), traces (list of matrices).
read_isodat_cf_raw_data <- function(json_path, gas_names) {
  if (is.null(gas_names) || length(gas_names) == 0L) {
    cli_abort("gas names must be known to read raw data from .cf files")
  }
  ctx_base <- "/CBlockData/0/objects/CBlockDataContext"

  if (length(gas_names) > 1L) {
    # multi-gas (case C): CBlockDataContext is an array, one entry per gas
    purrr::map2(
      seq_along(gas_names) - 1L,
      gas_names,
      function(i, gas) {
        scan <- query_json(
          json_path,
          paste0(
            ctx_base,
            "/",
            i,
            "/p/objects/CBlockData/objects/CRawDataScanStorage"
          )
        )
        binary <- scan$p$CBinary
        tibble::tibble(
          gas = gas,
          x = list(binary$x),
          traces = list(binary$traces)
        )
      }
    ) |>
      purrr::list_rbind()
  } else {
    # single gas: try CRawData first (case A), fall back to CRawDataScanStorage (case B)
    craw <- query_json(
      json_path,
      paste0(
        ctx_base,
        "/p/objects/CBlockDataContext/p/objects/CBlockData/objects/CRawData"
      ),
      required = FALSE
    )
    if (!json_missing(craw)) {
      eval_pp <- craw$p$CEvalGCData$p$p
      tibble::tibble(
        gas = gas_names[[1L]],
        x = list(eval_pp$x),
        traces = list(eval_pp$traces)
      )
    } else {
      scan <- query_json(
        json_path,
        paste0(ctx_base, "/p/objects/CBlockData/objects/CRawDataScanStorage")
      )
      binary <- scan$p$CBinary
      tibble::tibble(
        gas = gas_names[[1L]],
        x = list(binary$x),
        traces = list(binary$traces)
      )
    }
  }
}


## scn =====================

# .scn — scan file. No gas name or calibrated resistors; nominal values from CCupHardwarePart.
read_scn_json <- function(json_path) {
  # resistors (nominal, from CCupHardwarePart joined to active cups via CChannelGasConfPart)
  resistors <- read_isodat_scn_resistors(
    json_path,
    cup_hw_ptr = "/CScanStorage/CIntegrationUnitScanPart/p/CIntegrationUnitHardwarePart/CCupHardwarePart",
    channel_gas_ptr = "/CScanStorage/CGasConfiguration/p/objects/CIntegrationUnitGasConfPart/CChannelGasConfPart"
  ) |>
    try_catch_cnds()

  # file-level metadata: scan type, x unit, comment, timestamps
  file_info <- read_isodat_scn_file_info(json_path) |>
    try_catch_cnds()

  # raw scan data (resistors passed in to join mass by channel)
  raw_data <- read_isodat_scn_raw_data(json_path, resistors$result) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    resistors$conditions,
    file_info$conditions,
    raw_data$conditions
  )

  # return value
  tibble(
    resistors = list(resistors$result),
    file_info = list(file_info$result),
    raw_data = list(raw_data$result),
    problems = list(problems)
  )
}


# Detect which scan part is present and return its type label, x unit, and raw part object.
# Tries CScaleHvScanPart ("high voltage"), CMagnetCurrentScanPart ("magnet current"),
# CClockScanPart ("time"). Aborts if none is found.
read_isodat_scn_type <- function(json_path) {
  hv <-
    json_path |>
    query_json("/CScanStorage/CScaleHvScanPart", required = FALSE)
  if (!json_missing(hv)) {
    return(list(scan_type = "high voltage", x_unit = "kV", part = hv))
  }

  mag <- json_path |>
    query_json("/CScanStorage/CMagnetCurrentScanPart", required = FALSE)
  if (!json_missing(mag)) {
    return(list(scan_type = "magnet current", x_unit = "steps", part = mag))
  }

  clock <- json_path |>
    query_json("/CScanStorage/CClockScanPart", required = FALSE)
  if (!json_missing(clock)) {
    return(list(scan_type = "time", x_unit = "s", part = clock))
  }

  cli_abort("unrecognized scan type in {.path {json_path}}")
}

# Read scan file metadata: scan type, x axis unit, comment, and start/end timestamps.
# Returns a one-row tibble with columns type (fct), x_unit (chr), comment (chr),
# file_start (POSIXct UTC), file_end (POSIXct UTC).
read_isodat_scn_file_info <- function(json_path) {
  type_info <- read_isodat_scn_type(json_path)
  comment_raw <- json_path |>
    query_json("/CScanStorage/comment", required = FALSE)
  comment <- if (!json_missing(comment_raw) && nzchar(comment_raw)) {
    comment_raw
  } else {
    NA_character_
  }
  ts <- query_json(json_path, "/CScanStorage/timestamp_start")
  tibble::tibble(
    type = type_info$scan_type,
    x_units = type_info$x_unit,
    comment = comment,
    file_datetime = parse_isodat_datetime(ts)
  )
}


# Read raw scan data from a .scn JSON file.
# @param resistors tibble from read_isodat_scn_resistors() used to join mass by channel;
#   if NULL a warning is issued and mass is set to NA for all rows.
# Returns a long-format tibble with one row per (channel, scan-axis point):
#   channel      <int>  — 1-based trace/channel index
#   mass         <fct>  — m/z from resistors (NA if resistors unavailable)
#   type         <fct>  — scan type: "high_voltage", "magnet_current", or "time"
#   x_unit       <chr>  — physical unit for x: "kV", "steps", or "s"
#   x            <dbl>  — scan-axis value in physical units
#   intensity.mV <dbl>  — detector signal
read_isodat_scn_raw_data <- function(json_path, resistors = NULL) {
  binary <- query_json(json_path, "/CScanStorage/CBinary")
  type_info <- read_isodat_scn_type(json_path)
  x_raw <- binary$x
  x <- switch(
    type_info$scan_type,
    "high voltage" = {
      hw_pp <- type_info$part$p$CScaleHvHardwarePart$p$p
      x_raw /
        hw_pp$max_step *
        (hw_pp$max_value - hw_pp$min_value) /
        1000 +
        hw_pp$min_value / 1000
    },
    "magnet current" = as.numeric(x_raw),
    "time" = x_raw / 1000
  )
  n_traces <- nrow(binary$traces)
  out <- purrr::map(seq_len(n_traces), function(ch) {
    tibble::tibble(
      channel = ch,
      x = x,
      intensity.mV = binary$traces[ch, ]
    )
  }) |>
    purrr::list_rbind()

  if (is.null(resistors)) {
    cli_warn("resistors unavailable; {.field mass} set to NA for all channels")
    out <- dplyr::mutate(out, mass = as.factor(NA_character_), .after = channel)
  } else {
    out <- dplyr::left_join(
      out,
      dplyr::select(resistors, "channel", "mass"),
      by = "channel"
    ) |>
      dplyr::relocate(mass, .after = "channel")
  }
  out
}
