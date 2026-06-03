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
  # sub-methods only present in multi-gas files; list_as_tibble normalises single/multi uniformly
  subs <- query_json(
    json_path,
    sub_methods_ptr,
    required = FALSE,
    list_as_tibble = TRUE
  )
  sub_gas_names <- if (!json_missing(subs)) {
    vapply(subs$p, function(p) p$objects$CGasConfiguration$p$p$v, character(1))
  } else {
    character(0)
  }
  # should this be unique? probably yes, sometimes the same gas appears multiple
  # times in submethods but only one data block is generated for it
  c(top_gas, sub_gas_names) |> unique()
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
  resistors <- query_json(json_path, hw_list_ptr, list_as_tibble = TRUE)
  # safety checks
  if (length(gas_names) != nrow(resistors)) {
    cli_abort(
      "found {nrow(resistors)} {.field resistor set{?s}} but {length(gas_names)} {.field gas configurations{?s}} in the methods ({.emph {gas_names}})"
    )
  }
  # extra resistor info
  resistors$p |>
    purrr::map2(gas_names, extract_resistor_info) |>
    purrr::list_rbind()
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

  # raw trace data from RawDataBlock; gas_names and resistors matched positionally by gas index
  raw_data <- read_isodat_dxf_raw_data(
    json_path,
    gas_names$result,
    resistors$result
  ) |>
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
    traces = list(raw_data$result),
    problems = list(problems)
  )
}

# Read raw trace data from the RawDataBlock of a .dxf JSON file.
# gas_names and resistors are matched positionally: gas_names[[i]] and its rows in resistors
# correspond to the i-th CRawData entry, regardless of what complete_formula says.
# Returns a long-format tibble: analyte (fct), channel (int), mass (fct), time.s (dbl), intensity.mV (dbl).
# @param gas_names character vector from read_isodat_gas_names(); length must equal n raw data sets.
# @param resistors tibble from read_isodat_resistors(); if NULL warns and sets analyte/mass to NA.
read_isodat_dxf_raw_data <- function(json_path, gas_names, resistors = NULL) {
  if (is.null(resistors)) {
    cli_warn(
      "resistors unavailable; {.field analyte} and {.field mass} set to NA for all channels"
    )
  }
  # search for correct CBlockData object
  raw_idx <-
    json_path |>
    find_json_block_idx_by_label(
      "/CContiniousFlowBlockData/p/objects/CBlockData",
      label = "RawDataBlock"
    )
  craw <- json_path |>
    query_json(
      sprintf(
        "/CContiniousFlowBlockData/p/objects/CBlockData/%d/objects/CRawData",
        raw_idx
      ),
      list_as_tibble = TRUE
    )

  # safety checks
  if (length(gas_names) != nrow(craw)) {
    cli_abort(
      "found {nrow(craw)} {.field raw data set{?s}} but {length(gas_names)} {.field gas configurations{?s}} in the methods ({.emph {gas_names}})"
    )
  }

  craw$p |>
    purrr::map2(
      gas_names,
      function(p, gas) {
        eval_pp <- p$CEvalGCData$p$p
        res_for_gas <- if (!is.null(resistors)) {
          dplyr::filter(resistors, .data$analyte == !!gas)
        } else {
          NULL
        }
        expand_cf_and_dxf_traces(gas, eval_pp$x, eval_pp$traces, res_for_gas)
      }
    ) |>
    purrr::list_rbind()
}

# Expand one gas's trace matrix into a long-format tibble.
# Trace row i corresponds to res_for_gas row i (same JSON order).
# Returns: analyte (fct), channel (int), mass (fct), time.s (dbl), intensity.mV (dbl).
# res_for_gas is NULL when resistors are unavailable; analyte/mass are NA in that case.
expand_cf_and_dxf_traces <- function(gas, x, traces, res_for_gas) {
  n_channels <- nrow(traces)
  purrr::map(seq_len(n_channels), function(i) {
    if (is.null(res_for_gas)) {
      tibble::tibble(
        analyte = as.factor(NA_character_),
        channel = i,
        mass = as.factor(NA_character_),
        time.s = as.numeric(x),
        intensity.mV = as.numeric(traces[i, ])
      )
    } else {
      tibble::tibble(
        analyte = res_for_gas$analyte[i],
        channel = i,
        mass = res_for_gas$mass[i],
        time.s = as.numeric(x),
        intensity.mV = as.numeric(traces[i, ])
      )
    }
  }) |>
    purrr::list_rbind()
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

  # raw trace data (resistors passed in to join analyte/mass by channel row)
  raw_data <- read_isodat_cf_raw_data(
    json_path,
    gas_names$result,
    resistors$result
  ) |>
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
    traces = list(raw_data$result),
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
# Returns a long-format tibble: analyte (fct), channel (int), mass (fct), time.s (dbl), intensity.mV (dbl).
# @param resistors tibble from read_isodat_resistors(); if NULL warns and sets analyte/mass to NA.
read_isodat_cf_raw_data <- function(json_path, gas_names, resistors = NULL) {
  if (is.null(gas_names) || length(gas_names) == 0L) {
    cli_abort("gas names must be known to read raw data from .cf files")
  }
  if (is.null(resistors)) {
    cli_warn(
      "resistors unavailable; {.field analyte} and {.field mass} set to NA for all channels"
    )
  }
  ctx_base <- "/CBlockData/0/objects/CBlockDataContext"

  expand_gas <- function(gas, x, traces) {
    res_for_gas <- if (!is.null(resistors)) {
      dplyr::filter(resistors, analyte == gas)
    } else {
      NULL
    }
    expand_cf_and_dxf_traces(gas, x, traces, res_for_gas)
  }

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
        expand_gas(gas, binary$x, binary$traces)
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
      expand_gas(gas_names[[1L]], eval_pp$x, eval_pp$traces)
    } else {
      scan <- query_json(
        json_path,
        paste0(ctx_base, "/p/objects/CBlockData/objects/CRawDataScanStorage")
      )
      binary <- scan$p$CBinary
      expand_gas(gas_names[[1L]], binary$x, binary$traces)
    }
  }
}

## did ===========

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

  # cycle data (resistors passed to name intensity columns by mass)
  cycles <- read_isodat_di_cycles(
    json_path,
    root_ptr = "/CDualInletBlockData",
    resistors = resistors$result
  ) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    gas_names$conditions,
    resistors$conditions,
    cycles$conditions
  )

  # return value
  tibble(
    resistors = list(resistors$result),
    cycles = list(cycles$result),
    problems = list(problems)
  )
}

# Read dual-inlet cycle data from a .did or .caf JSON file.
# Both file types share the same CDualInletRawData structure; only the root_ptr differs.
# Returns a long-format tibble: type (chr), cycle (int), analyte (fct), mass (fct), intensity.mV (dbl).
# Cycle 0 is the standard pre-cycle; subsequent cycles are 1-indexed per type.
# @param root_ptr "/CDualInletBlockData" for .did, "/CBlockDataContext" for .caf.
# @param resistors tibble from read_isodat_resistors(); analyte/mass are NA for all rows if NULL.
read_isodat_di_cycles <- function(json_path, root_ptr, resistors = NULL) {
  base <- paste0(root_ptr, "/p/objects/CDualInletRawData/p/objects")

  # helper: extract list of numeric intensity vectors; list_as_tibble normalises single/multi
  extract_rows <- function(ptr) {
    iut <- query_json(json_path, paste0(base, ptr), list_as_tibble = TRUE)
    purrr::map(iut$CIntensityData, function(cid) as.numeric(unlist(cid$values)))
  }

  pre_rows <- extract_rows("/CIntegrationUnitTransferPart")
  pre_row <- pre_rows[[1L]]
  std_rows <- extract_rows("/CBlockData/0/objects/CIntegrationUnitTransferPart")
  smp_rows <- extract_rows("/CBlockData/1/objects/CIntegrationUnitTransferPart")

  # analyte and mass labels: from resistors (in channel order) or NA fallback
  if (is.null(resistors)) {
    cli_warn(
      "resistors unavailable; {.field analyte} and {.field mass} set to NA for all channels"
    )
    analyte_vals <- as.factor(rep(NA_character_, length(pre_row)))
    mass_vals <- as.factor(rep(NA_character_, length(pre_row)))
  } else {
    analyte_vals <- resistors$analyte
    mass_vals <- resistors$mass
  }

  # assemble long-format tibble
  n_std <- length(std_rows)
  n_smp <- length(smp_rows)
  all_types <- c("standard", rep("standard", n_std), rep("sample", n_smp))
  all_cycles <- c(0L, seq_len(n_std), seq_len(n_smp))
  all_values <- c(list(pre_row), std_rows, smp_rows)

  purrr::map2(
    all_types,
    seq_along(all_types),
    function(type, i) {
      tibble::tibble(
        analyte = analyte_vals,
        type = type,
        cycle = all_cycles[i],
        mass = mass_vals,
        intensity.mV = all_values[[i]]
      )
    }
  ) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$cycle, .data$type, .data$mass)
}


## caf ===================

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

  # cycle data (resistors passed to name intensity columns by mass)
  cycles <- read_isodat_di_cycles(
    json_path,
    root_ptr = "/CBlockDataContext",
    resistors = resistors$result
  ) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    gas_names$conditions,
    resistors$conditions,
    cycles$conditions
  )

  # return value
  tibble(
    resistors = list(resistors$result),
    cycles = list(cycles$result),
    problems = list(problems)
  )
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
    traces = list(raw_data$result),
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
