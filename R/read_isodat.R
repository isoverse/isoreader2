# general isodat readers ===================

# Read gas names from CGasConfiguration/p/p/v. For multi-gas files, also reads
# each sub-method's gas name and de-duplicates.
read_isodat_gas_names <- function(
  json_path,
  gas_name_ptr,
  sub_methods_ptr = NULL
) {
  top_gas <- query_json(json_path, gas_name_ptr)
  # sub-methods only present in multi-gas files; list_as_tibble normalises single/multi uniformly
  sub_gas_names <- if (!is.null(sub_methods_ptr)) {
    subs <- query_json(
      json_path,
      sub_methods_ptr,
      required = FALSE,
      list_as_tibble = TRUE
    )
    if (!json_missing(subs)) {
      vapply(
        subs$p,
        function(p) p$objects$CGasConfiguration$p$p$v,
        character(1)
      )
    } else {
      character(0)
    }
  } else {
    character(0)
  }
  # should this be unique? sometimes the same gas appears multiple
  # times in submethods but only one data block is generated for it
  # --> later join between raw data and resistor information depends on this
  c(top_gas, sub_gas_names) |> unique()
}

# Read calibrated Faraday cup resistors from CEvalIntegrationUnitHWInfoList.
# Returns a tibble with columns: species, mass, channel (1-based), cup, resistor.
read_isodat_calibrated_resistors <- function(
  json_path,
  hw_list_ptr,
  gas_names
) {
  resistors <- query_json(json_path, hw_list_ptr, list_as_tibble = TRUE)
  # safety checks
  if (length(gas_names) != nrow(resistors)) {
    cli_abort(
      "found {nrow(resistors)} {.field resistor set{?s}} but {length(gas_names)} {.field gas configurations{?s}} in the methods ({.emph {gas_names}})"
    )
  }
  # extra resistor info
  resistors$p |>
    purrr::map2(gas_names, function(parent_node, gas) {
      hw <- parent_node$objects$CEvalIntegrationUnitHWInfo
      tibble::tibble(
        species = as.character(gas),
        mass = as.character(hw$mass),
        channel = as.integer(hw$channel) + 1L, # 0-based, make 1-based as is customary in R
        cup = as.integer(hw$cup),
        resistor = as.numeric(hw$resistor)
      )
    }) |>
    purrr::list_rbind()
}

# Read nominal Faraday cup resistors from CCupHardwarePart (.scn only).
# Active cups and their mass/channel assignments come from CChannelGasConfPart;
# resistor values are matched to cups by position index.
# Returns a tibble with columns: mass, channel (1-based), cup, resistor.
read_isodat_nominal_resistors <- function(
  json_path,
  cup_hw_ptr,
  channel_gas_ptr
) {
  hw <- query_json(json_path, cup_hw_ptr)
  channels <- query_json(json_path, channel_gas_ptr)
  # resistor info
  tibble::tibble(
    mass = as.character(channels$mass),
    channel = as.integer(channels$idx), # already 1-based (as is customary in R)
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

# Read file-level metadata from CFileHeader (all file types except .scn).
# Returns a one-row tibble with file_datetime (POSIXct UTC).
read_isodat_file_info <- function(json_path) {
  ts <- query_json(json_path, "/CFileHeader/p/objects/CTimeObject/datetime")
  tibble::tibble(file_datetime = parse_isodat_datetime(ts))
}

# Parse a trace matrix into a long-format tibble.
# Returns: channel (int), x (dbl), intensity.mV (dbl).
parse_isodat_traces <- function(x, traces) {
  traces |>
    nrow() |>
    seq_len() |>
    purrr::map(function(i) {
      tibble::tibble(
        channel = i,
        x = as.numeric(x),
        intensity.mV = as.numeric(traces[i, ])
      )
    }) |>
    purrr::list_rbind()
}

# Read dual-inlet cycle data from CDualInletRawData (.did and .caf share this structure).
# Cycle 0 is the standard pre-cycle; subsequent cycles are 1-indexed per type.
# Standard and sample CBlockData children are located by their p/v label.
# @param root_ptr "/CDualInletBlockData" for .did, "/CBlockDataContext" for .caf.
parse_isodat_cycles <- function(json_path, root_ptr) {
  # base query path
  base <- sprintf("%s/p/objects/CDualInletRawData/p/objects", root_ptr)

  # get pre row
  pre_row <-
    sprintf("%s/CIntegrationUnitTransferPart", base) |>
    query_json(json_path = json_path, list_as_tibble = TRUE) |>
    dplyr::pull(.data$CIntensityData) |>
    purrr::map(~ .x$values)

  # safety check
  if (length(pre_row) != 1) {
    cli_abort("expected 1 initial pre-analysis but found {length(pre_row)}")
  }

  # locate standard and sample blocks by their p/v label
  std_idx <- find_json_block_idx_by_value(
    json_path,
    sprintf("%s/CBlockData", base),
    "DualInlet RawData Standard Block"
  )
  sample_idx <- find_json_block_idx_by_value(
    json_path,
    sprintf("%s/CBlockData", base),
    "DualInlet RawData Sample Block"
  )

  # pull out data
  std_rows <-
    sprintf(
      "%s/CBlockData/%d/objects/CIntegrationUnitTransferPart",
      base,
      std_idx
    ) |>
    query_json(json_path = json_path, list_as_tibble = TRUE) |>
    dplyr::pull(.data$CIntensityData) |>
    purrr::map(~ .x$values)
  sample_rows <-
    sprintf(
      "%s/CBlockData/%d/objects/CIntegrationUnitTransferPart",
      base,
      sample_idx
    ) |>
    query_json(json_path = json_path, list_as_tibble = TRUE) |>
    dplyr::pull(.data$CIntensityData) |>
    purrr::map(~ .x$values)

  # safety check
  if (length(std_rows) != length(sample_rows)) {
    cli_abort(
      "expected the same number of cycles for standards ({length(std_rows)}) and samples ({length(sample_rows)})"
    )
  }

  # assemble long-format tibble
  n_std <- length(std_rows)
  n_smp <- length(sample_rows)
  all_types <- c("standard", rep("standard", n_std), rep("sample", n_smp))
  all_cycles <- c(0L, seq_len(n_std), seq_len(n_smp))
  all_values <- c(pre_row, std_rows, sample_rows)

  # return
  list(
    type = all_types,
    cycle = all_cycles,
    values = all_values
  ) |>
    purrr::pmap(
      function(type, cycle, values) {
        tibble::tibble(
          cycle = cycle,
          type = type,
          channel = seq_along(values),
          intensity.mV = values
        )
      }
    ) |>
    purrr::list_rbind() |>
    dplyr::arrange(.data$cycle, .data$type, .data$channel)
}

# Join long-format data with resistors on (species, channel) to add the mass column.
# Warns if any data channels lack a resistor entry or any resistor channels have no data.
match_channels_to_masses <- function(data, resistors) {
  # is there data?
  if (is.null(data)) {
    return(NULL)
  }

  # are there resistors?
  if (is.null(resistors) || nrow(resistors) == 0) {
    cli_warn(
      "resistors unavailable; {.field mass} set to NA for all channels"
    )
    return(data |> dplyr::mutate(mass = NA_character_, .after = "channel"))
  }

  # join
  data_w_masses <-
    data |>
    dplyr::mutate(..id = dplyr::row_number()) |>
    dplyr::full_join(
      resistors |> dplyr::select("species", "channel", "mass"),
      by = c("species", "channel")
    ) |>
    dplyr::relocate("mass", .after = "channel")

  # any warnings required?
  if (any(is.na(data_w_masses$..id))) {
    # some resistor channels do not actually have data!
    missing_channels <- data_w_masses |>
      dplyr::filter(is.na(.data$..id)) |>
      dplyr::count(.data$species, .data$mass) |>
      dplyr::summarize(
        .by = "species",
        info = format_inline(
          "{col_magenta(species[1])} is missing {sum(n)} measurements (for masses {.field {unique(mass)}})"
        )
      )
    cli_warn("{missing_channels$info}")
  }
  if (any(is.na(data_w_masses$mass))) {
    # some data channels don't have resitors!
    missing_masses <- data_w_masses |>
      dplyr::filter(is.na(.data$mass)) |>
      dplyr::count(.data$species, .data$channel) |>
      dplyr::summarize(
        .by = "species",
        info = format_inline(
          "{col_magenta(species[1])} is missing mass information for channels {.field {unique(channel)}}"
        )
      )
    cli_warn("{missing_masses$info}")
  }

  # return
  data_w_masses |> dplyr::filter(!is.na(.data$..id)) |> dplyr::select(-"..id")
}

# Read sequence line information from the "Sequence Line Information" CBlockData (.dxf, .did).
# CData rows supply field names (v) and values (l). Returns a one-row tibble with verbatim
# field names; duplicate names are dropped (first kept). Aborts if the block is absent.
read_isodat_seq_line_info <- function(json_path, block_query) {
  seq_idx <- find_json_block_idx_by_value(
    json_path,
    block_query,
    "Sequence Line Information"
  )
  entries <- query_json(
    json_path,
    sprintf("%s/%d/objects/CData", block_query, seq_idx),
    list_as_tibble = TRUE
  )
  keep <- !duplicated(entries$v)
  tibble::as_tibble_row(as.list(setNames(
    as.character(entries$l[keep]),
    entries$v[keep]
  )))
}

# Read sequence line information from CSequenceLineInformationGridStorage/p/CGridCtrl/cells (.cf, .caf).
# cells is a 2-row character matrix: row 1 = headers, row 2 = values.
# Returns a one-row tibble with verbatim header names; duplicate names are dropped (first kept).
# Returns NULL when the node is absent.
read_isodat_seq_line_grid <- function(json_path, grid_ptr) {
  cells <- query_json(json_path, grid_ptr, required = FALSE)
  if (json_missing(cells)) {
    return(NULL)
  }
  keep <- !duplicated(cells[1L, ])
  tibble::as_tibble_row(as.list(setNames(cells[2L, keep], cells[1L, keep])))
}

# file type specific readers ==========

## dxf =====================

# .dxf — continuous-flow file (CContiniousFlowBlockData root).
# Returns file_info, seq_info, resistors (calibrated), traces.
read_dxf_json <- function(json_path) {
  # file-level metadata (datetime from CFileHeader)
  file_info <- read_isodat_file_info(json_path) |> try_catch_cnds()

  # sequence line information (sample metadata)
  seq_info <- json_path |>
    read_isodat_seq_line_info(
      "/CContiniousFlowBlockData/p/objects/CBlockData"
    ) |>
    try_catch_cnds()

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
  resistors <- read_isodat_calibrated_resistors(
    json_path,
    hw_list_ptr = "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
    gas_names = gas_names$result
  ) |>
    try_catch_cnds()

  # raw trace data from RawDataBlock; gas_names and resistors matched positionally by gas index
  raw_data <- read_isodat_dxf_raw_data(json_path, gas_names$result) |>
    match_channels_to_masses(resistors$result) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    file_info$conditions,
    seq_info$conditions,
    gas_names$conditions,
    resistors$conditions,
    raw_data$conditions
  )

  # return value
  tibble(
    file_info = list(file_info$result),
    seq_info = list(seq_info$result),
    resistors = list(resistors$result),
    traces = list(raw_data$result),
    problems = list(problems)
  )
}

# Read raw trace data from the "RawDataBlock" CBlockData of a .dxf JSON file.
# gas_names[i] is assigned to the i-th CRawData entry. Returns a long-format tibble:
# species (chr), channel (int), time.s (dbl), intensity.mV (dbl).
read_isodat_dxf_raw_data <- function(json_path, gas_names) {
  # search for correct CBlockData object
  raw_idx <-
    json_path |>
    find_json_block_idx_by_value(
      "/CContiniousFlowBlockData/p/objects/CBlockData",
      value = "RawDataBlock"
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

  # parse
  craw$p |>
    purrr::map2(
      gas_names,
      function(craw_parent, gas) {
        parse_isodat_traces(
          craw_parent$CEvalGCData$p$p$x,
          craw_parent$CEvalGCData$p$p$traces
        ) |>
          dplyr::rename("time.s" = "x") |>
          dplyr::mutate(species = !!gas, .before = 1L)
      }
    ) |>
    purrr::list_rbind()
}

## cf ===============

# .cf — continuous-flow file (CMethod root).
# Returns file_info, seq_info, resistors (calibrated), traces.
read_cf_json <- function(json_path) {
  # file-level metadata (datetime from CFileHeader)
  file_info <- read_isodat_file_info(json_path) |> try_catch_cnds()

  # sequence line information (sample metadata, absent in some files)
  seq_info <- json_path |>
    read_isodat_seq_line_grid(
      grid_ptr = "/CBlockData/1/objects/CSequenceLineInformationGridStorage/p/CGridCtrl/cells"
    ) |>
    try_catch_cnds()

  # gas names
  gas_names <- read_isodat_gas_names(
    json_path,
    gas_name_ptr = "/CMethod/p/objects/CGasConfiguration/p/p/v",
    sub_methods_ptr = "/CMethod/CMethod"
  ) |>
    try_catch_cnds()

  # resistors
  resistors <- read_isodat_calibrated_resistors(
    json_path,
    hw_list_ptr = "/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
    gas_names = gas_names$result
  ) |>
    try_catch_cnds()

  # raw trace data (resistors passed in to join species/mass by channel row)
  raw_data <- read_isodat_cf_raw_data(json_path, gas_names$result) |>
    match_channels_to_masses(resistors$result) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    file_info$conditions,
    seq_info$conditions,
    gas_names$conditions,
    resistors$conditions,
    raw_data$conditions
  )

  # return value
  tibble(
    file_info = list(file_info$result),
    seq_info = list(seq_info$result),
    resistors = list(resistors$result),
    traces = list(raw_data$result),
    problems = list(problems)
  )
}

# Read raw trace data from a .cf JSON file. Gas names must be supplied externally;
# the data blocks carry no gas identity. Tries three structural variants in order
# (multi-gas indexed, single-gas ScanStorage, single-gas CRawData).
read_isodat_cf_raw_data <- function(json_path, gas_names) {
  # safety checks
  if (length(gas_names) == 0L) {
    cli_abort("gas names must be known to read raw data from .cf files")
  }
  ctx_base <- "/CBlockData/0/objects/CBlockDataContext"

  # For each gas at 0-based index i, try the three structural variants in order:
  #   C) multi-gas:   ctx_base/{i}/p/objects/CBlockData/objects/CRawDataScanStorage/p/CBinary
  #   B) single-gas ScanStorage: ctx_base/p/objects/CBlockData/objects/CRawDataScanStorage/p/CBinary
  #   A) single-gas CRawData:    ctx_base/p/objects/CBlockDataContext/.../CRawData/p/CEvalGCData/p/p
  # All three variants resolve to a node with {x, traces}; list_as_tibble normalises to a tibble.
  gas_names |>
    seq_along() |>
    purrr::map2(
      gas_names,
      function(i, gas) {
        binary <- query_json(
          json_path,
          c(
            sprintf(
              "%s/%d/p/objects/CBlockData/objects/CRawDataScanStorage/p/CBinary",
              ctx_base,
              i - 1L # index is 0 based
            ),
            sprintf(
              "%s/p/objects/CBlockData/objects/CRawDataScanStorage/p/CBinary",
              ctx_base
            ),
            sprintf(
              "%s/p/objects/CBlockDataContext/p/objects/CBlockData/objects/CRawData/p/CEvalGCData/p/p",
              ctx_base
            )
          ),
          list_as_tibble = TRUE
        )
        parse_isodat_traces(
          binary$x[[1L]],
          binary$traces[[1L]]
        ) |>
          dplyr::rename("time.s" = "x") |>
          dplyr::mutate(species = !!gas, .before = 1L)
      }
    ) |>
    purrr::list_rbind()
}

## did ===========

# .did — dual-inlet file (CDualInletBlockData root).
# Returns file_info, seq_info, resistors (calibrated), cycles.
# CEvalIntegrationUnitHWInfoStore may be wrapped in CNumericValue (version-dependent).
read_did_json <- function(json_path) {
  # file-level metadata (datetime from CFileHeader)
  file_info <- read_isodat_file_info(json_path) |> try_catch_cnds()

  # sequence line information (sample metadata)
  seq_info <-
    json_path |>
    read_isodat_seq_line_info("/CDualInletBlockData/p/objects/CBlockData") |>
    try_catch_cnds()

  # gas names
  gas_name <- read_isodat_gas_names(
    json_path,
    gas_name_ptr = "/CDualInletBlockData/p/objects/CMethod/p/objects/CGasConfiguration/p/p/v",
    sub_methods_ptr = "/CDualInletBlockData/p/objects/CMethod/CMethod"
  ) |>
    try_catch_cnds()

  # resistors
  resistors <-
    read_isodat_calibrated_resistors(
      json_path,
      # two variants: direct path tried first, CNumericValue-wrapped as fallback
      hw_list_ptr = c(
        "/CDualInletBlockData/p/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
        "/CDualInletBlockData/p/objects/CMethod/p/objects/CNumericValue/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList"
      ),
      gas_names = gas_name$result
    ) |>
    try_catch_cnds()

  # cycle data
  cycles <-
    parse_isodat_cycles(json_path, root_ptr = "/CDualInletBlockData") |>
    dplyr::mutate(species = gas_name$result, .before = 1L) |>
    match_channels_to_masses(resistors$result) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    file_info$conditions,
    seq_info$conditions,
    gas_name$conditions,
    resistors$conditions,
    cycles$conditions
  )

  # return value
  tibble(
    file_info = list(file_info$result),
    seq_info = list(seq_info$result),
    resistors = list(resistors$result),
    cycles = list(cycles$result),
    problems = list(problems)
  )
}


## caf ===================

# .caf — dual-inlet file (CBlockDataContext root).
# Returns file_info, seq_info, resistors (calibrated), cycles.
read_caf_json <- function(json_path) {
  # file-level metadata (datetime from CFileHeader)
  file_info <- read_isodat_file_info(json_path) |> try_catch_cnds()

  # sequence line information (sample metadata, absent in some files)
  seq_info <-
    json_path |>
    read_isodat_seq_line_grid(
      "/CBlockDataContext/p/objects/CSequenceLineInformationGridStorage/p/CGridCtrl/cells"
    ) |>
    try_catch_cnds()

  # gas names
  gas_name <- read_isodat_gas_names(
    json_path,
    gas_name_ptr = "/CBlockDataContext/p/objects/CMethod/p/objects/CGasConfiguration/p/p/v",
    sub_methods_ptr = "/CBlockDataContext/p/objects/CMethod/CMethod"
  ) |>
    try_catch_cnds()

  # resistors
  resistors <- read_isodat_calibrated_resistors(
    json_path,
    hw_list_ptr = "/CBlockDataContext/p/objects/CMethod/p/objects/CEvalIntegrationUnitHWInfoStore/p/objects/CEvalIntegrationUnitHWInfoList",
    gas_names = gas_name$result
  ) |>
    try_catch_cnds()

  # cycle data
  cycles <-
    parse_isodat_cycles(json_path, root_ptr = "/CBlockDataContext") |>
    dplyr::mutate(species = gas_name$result, .before = 1L) |>
    match_channels_to_masses(resistors$result) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    file_info$conditions,
    seq_info$conditions,
    gas_name$conditions,
    resistors$conditions,
    cycles$conditions
  )

  # return value
  tibble(
    file_info = list(file_info$result),
    seq_info = list(seq_info$result),
    resistors = list(resistors$result),
    cycles = list(cycles$result),
    problems = list(problems)
  )
}

## scn =====================

# .scn — scan file (CScanStorage root). No calibrated resistors; uses nominal values from CCupHardwarePart.
# Returns file_info (type, x_units, comment, file_datetime), resistors (nominal), traces.
read_scn_json <- function(json_path) {
  # file-level metadata: scan type, x unit, comment, timestamps
  file_info <- read_isodat_scn_file_info(json_path) |>
    try_catch_cnds()

  # gas name (no sub-methods in scan files)
  gas_name <- read_isodat_gas_names(
    json_path,
    gas_name_ptr = "/CScanStorage/CGasConfiguration/p/p/v"
  ) |>
    try_catch_cnds(error_value = NA_character_)

  # nominal resistors (all physical cups, from CCupHardwarePart)
  nominal_resistors <- read_isodat_nominal_resistors(
    json_path,
    cup_hw_ptr = "/CScanStorage/CIntegrationUnitScanPart/p/CIntegrationUnitHardwarePart/CCupHardwarePart",
    channel_gas_ptr = "/CScanStorage/CGasConfiguration/p/objects/CIntegrationUnitGasConfPart/CChannelGasConfPart"
  ) |>
    # add species since nominal resistors are not species specific
    dplyr::mutate(species = gas_name$result, .before = 1L) |>
    try_catch_cnds()

  # raw scan data (nominal_resistors passed in to join mass by channel)
  raw_data <- read_isodat_scn_raw_data(json_path) |>
    # add species since nominal raw data is not species specific
    dplyr::mutate(species = gas_name$result, .before = 1L) |>
    match_channels_to_masses(nominal_resistors$result) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    file_info$conditions,
    gas_name$conditions,
    nominal_resistors$conditions,
    raw_data$conditions
  )

  # return value
  tibble(
    file_info = list(file_info$result),
    resistors = list(nominal_resistors$result),
    traces = list(raw_data$result),
    problems = list(problems)
  )
}


# Detect the scan type from CScanStorage. Returns list(scan_type, x_unit, part).
# Tries CScaleHvScanPart ("high voltage"), CMagnetCurrentScanPart ("magnet current"),
# CClockScanPart ("time") in order; aborts if none is present.
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

# Read .scn file metadata from CScanStorage. Returns a one-row tibble:
# file_datetime (POSIXct UTC), type (chr), x_units (chr), comment (chr).
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
    file_datetime = parse_isodat_datetime(ts),
    type = type_info$scan_type,
    x_units = type_info$x_unit,
    comment = comment
  )
}

# Read raw scan data from CScanStorage/CBinary. Converts x from raw steps to physical units
# (kV for high-voltage, steps for magnet current, s for time).
# Returns a long-format tibble: channel (int), x (dbl), intensity.mV (dbl).
read_isodat_scn_raw_data <- function(json_path) {
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

  # parse traces
  parse_isodat_traces(x = x, traces = binary$traces)
}
