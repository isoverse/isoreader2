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
# Returns a tibble with columns: species, channel (1-based), cup, mass, resistance.Ohm.
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
      tibble(
        species = as.character(gas),
        channel = as.integer(hw$channel) + 1L, # 0-based, make 1-based as is customary in R
        mass = as.character(signif(as.numeric(hw$mass), digits = 3)),
        cup = as.integer(hw$cup),
        resistance.Ohm = as.numeric(hw$resistor)
      )
    }) |>
    purrr::list_rbind()
}

# Read nominal Faraday cup resistors from CCupHardwarePart (.scn only).
# Active cups and their mass/channel assignments come from CChannelGasConfPart;
# resistor values are matched to cups by position index.
# Returns a tibble with columns: channel (1-based), cup, mass, resistance.Ohm.
read_isodat_nominal_resistors <- function(
  json_path,
  cup_hw_ptr,
  channel_gas_ptr
) {
  hw <- query_json(json_path, cup_hw_ptr)
  channels <- query_json(json_path, channel_gas_ptr)
  # resistor info
  tibble(
    channel = as.integer(channels$idx), # already 1-based (as is customary in R)
    mass = as.character(signif(as.numeric(channels$mass), digits = 3)),
    cup = as.integer(channels$cup),
    resistance.Ohm = as.numeric(hw$resistor[match(channels$cup, hw$idx)])
  )
}

# Parse an ISO 8601 datetime string (with optional timezone offset) to POSIXct UTC.
parse_iso8601_datetime <- function(x) {
  if (is_empty(x)) {
    return(as.POSIXct(NA))
  }
  as.POSIXct(
    sub("[+-]\\d{2}:\\d{2}$", "", x),
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )
}

# Read datetime (POSIXct UTC).
read_isodat_timestamp <- function(json_path) {
  ts <- query_json(json_path, "/CFileHeader/p/objects/CTimeObject/datetime")
  parse_iso8601_datetime(ts)
}

# Reads h3_factor from CH3FactorResult under any of the supplied CGasConfiguration paths.
# Returns NA_real_ when absent (non-H2 measurement).
read_isodat_h3_factor <- function(json_path, gas_config_ptrs) {
  for (ptr in gas_config_ptrs) {
    h3 <- query_json(
      json_path,
      paste0(ptr, "/p/objects/CH3FactorResult/h3_factor"),
      required = FALSE
    )
    if (!json_missing(h3)) {
      return(as.numeric(h3))
    }
  }
  NA_real_
}

# Parse a trace matrix into a long-format tibble.
parse_isodat_traces <- function(x, traces) {
  traces |>
    nrow() |>
    seq_len() |>
    purrr::map(function(i) {
      tibble(
        analysis = 1L, # always a single analysis
        channel = i,
        x = as.numeric(x),
        intensity.V = as.numeric(traces[i, ]) / 1000.
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
        tibble(
          analysis = 1L, # always a single analysis
          cycle = cycle,
          type = type,
          channel = seq_along(values),
          intensity.V = values / 1000.
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
  tibble::as_tibble_row(as.list(set_names(
    as.character(entries$l[keep]),
    entries$v[keep]
  ))) |>
    dplyr::mutate(analysis = 1L, .before = 1L) # always a single analysis
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
  tibble::as_tibble_row(as.list(set_names(cells[2L, keep], cells[1L, keep]))) |>
    dplyr::mutate(analysis = 1L, .before = 1L) # always a single analysis
}

# vendor data table readers ==========
#
# These read the computed/evaluated "vendor data table" that Isodat displays in
# its results view (peak-by-peak results for continuous flow, cycle-by-cycle
# results for dual inlet), stored per file as `vendor_data_table` (only the
# isodat .dxf/.cf/.did/.caf formats carry one).
#
# JSON locations (mapped from the old isoreader binary C-block navigation):
#   .dxf  -> CResultArray/.../CResultForGas/{gas}/CGCPeakList/.../CSPeak/{i}
#   .cf   -> CBlockDataContext/.../CGCPeakList/.../CSPeak/{i}
#   .caf  -> CResultDataList/{gas}/.../CResultData/{i}
#   .did  -> CDualInletEvaluatedDataCollect/.../CDualInletEvaluatedData/{col}
#
# For .dxf/.cf/.caf each row's cells live under a CEvalDataItemListTransferPart
# as typed CEvalData{Double,Int,String}TransferPart entries — numeric (Double,
# Int) and string (e.g. "Ref. Name") columns are all read. Rows are enumerated
# by index via scalar pointers (n_objects / gas_name).

# Read the cells of one CEvalDataItemListTransferPart `objects` node into a named
# list (column name -> value), ordered by the cells' idx. Double/Int cells become
# numeric values, String cells character values (so the returned list is mixed
# type). For Double/Int the value is at `p/data`; for String at `data_string`;
# the column name is at `p/p/name` and its (already bracketed) units, when any,
# at `p/p/units` — these are combined into "name [units]".
read_isodat_eval_cells <- function(json_path, objects_ptr) {
  # normalise an RcppSimdJson result (a data frame of cells for multiple, or a
  # single named list for one) to a plain list of per-cell objects
  as_cell_list <- function(parts) {
    if (is.data.frame(parts)) {
      lapply(seq_len(nrow(parts)), function(i) {
        cell <- as.list(parts[i, ])
        cell$p <- cell$p[[1]] # unwrap the length-1 list-column element
        cell
      })
    } else {
      list(parts)
    }
  }
  collect <- function(type, value_fun) {
    parts <- query_json(
      json_path,
      paste0(objects_ptr, "/", type),
      required = FALSE
    )
    if (json_missing(parts)) {
      return(NULL)
    }
    cells <- as_cell_list(parts)
    tibble(
      idx = as.integer(vapply(
        cells,
        function(c) c$idx %||% NA_integer_,
        numeric(1)
      )),
      # append the (already bracketed) units to the column name when present,
      # e.g. "rIntensity 28" + "[mVs]" -> "rIntensity 28 [mVs]"
      name = vapply(
        cells,
        function(c) {
          nm <- trimws(c$p$p$name %||% NA_character_)
          un <- trimws(c$p$p$units %||% "")
          if (!is.na(nm) && nzchar(un)) paste0(nm, " ", un) else nm
        },
        character(1)
      ),
      value = lapply(cells, value_fun)
    )
  }
  cells <- dplyr::bind_rows(
    collect("CEvalDataDoubleTransferPart", function(c) {
      as.numeric(c$p$data %||% NA_real_)
    }),
    collect("CEvalDataIntTransferPart", function(c) {
      as.integer(c$p$data %||% NA_integer_)
    }),
    collect("CEvalDataStringTransferPart", function(c) {
      as.character(c$data_string %||% NA_character_)
    })
  )
  if (is.null(cells) || nrow(cells) == 0L) {
    return(NULL)
  }
  cells <- cells[order(cells$idx), ]
  cells <- cells[
    !is.na(cells$name) & nzchar(cells$name) & !duplicated(cells$name),
  ]
  stats::setNames(cells$value, cells$name)
}

# Read a CGCPeakList into a data table (one row per CSPeak). `species` comes from
# each peak's gas_name. Returns NULL if the peak list is absent/empty.
read_isodat_gc_peak_table <- function(json_path, peaklist_ptr) {
  n_peaks <- query_json(
    json_path,
    paste0(peaklist_ptr, "/p/n_objects"),
    required = FALSE
  )
  if (json_missing(n_peaks) || as.integer(n_peaks) == 0L) {
    return(NULL)
  }
  rows <- purrr::map(seq_len(as.integer(n_peaks)) - 1L, function(pk) {
    cspeak_ptr <- sprintf("%s/p/objects/CSPeak/%d", peaklist_ptr, pk)
    species <- query_json(
      json_path,
      sprintf("%s/p/gas_name", cspeak_ptr),
      required = FALSE
    )
    # retention-time / amplitude / background geometry (stored separately from
    # the evaluated cells, in the per-trace CGCPeak objects)
    geometry <- read_isodat_gc_peak_geometry(json_path, cspeak_ptr)
    # evaluated results table cells
    cells <- read_isodat_eval_cells(
      json_path,
      sprintf("%s/p/CEvalDataItemListTransferPart/p/objects", cspeak_ptr)
    )
    if (is.null(cells) && is.null(geometry)) {
      return(NULL)
    }
    tibble::as_tibble_row(c(
      list(
        analysis = 1L,
        species = if (json_missing(species)) {
          NA_character_
        } else {
          as.character(species)
        }
      ),
      geometry,
      cells
    ))
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0L) NULL else out
}

# Read the retention-time geometry of one CSPeak from its per-trace CGCPeak
# objects: shared chromatographic window `Start`/`Rt`/`End` (seconds, taken from
# the first/major trace, as Isodat reports it) plus per-mass apex amplitude
# (`Ampl <mass>`) and background (`BGD <mass>`) in mV. In debug mode
# (`ir_get_option("debug")`) the peak's `square peak` flag and per-mass time
# shifts (`Shift <mass> [s]`) are also included. Returns NULL if absent.
read_isodat_gc_peak_geometry <- function(json_path, cspeak_ptr) {
  gc <- query_json(
    json_path,
    sprintf("%s/p/p/objects/CGCPeak", cspeak_ptr),
    required = FALSE,
    list_as_tibble = TRUE
  )
  if (json_missing(gc)) {
    return(NULL)
  }
  # get out the peak information
  p <- gc$p
  start_rt <- gc$start_rt
  apex_rt <- gc$apex_rt
  end_rt <- gc$end_rt
  apex_signal <- gc$apex_signal
  mass <- vapply(
    p,
    function(x) as.character(x$mass %||% NA_character_),
    character(1)
  )
  bgd <- vapply(p, function(x) as.numeric(x$bgd1 %||% NA_real_), numeric(1))
  if (length(mass) == 0L) {
    return(NULL)
  }
  geometry <- c(
    list(
      `Start [s]` = as.numeric(start_rt[1]),
      `Rt [s]` = as.numeric(apex_rt[1]),
      `End [s]` = as.numeric(end_rt[1])
    ),
    stats::setNames(
      as.list(as.numeric(apex_signal)),
      sprintf("Ampl %s [mV]", mass)
    ),
    stats::setNames(as.list(bgd), sprintf("BGD %s [mV]", mass))
  )

  # debug-only extras: the square-peak flag (0/1 -> FALSE/TRUE) and the per-mass
  # time shifts (`Shift <mass> [s]`)
  if (ir_get_option("debug")) {
    geometry <- c(
      geometry,
      list(`square peak` = as.logical(as.numeric(gc$square_peak[1] %||% NA))),
      stats::setNames(
        as.list(as.numeric(gc$time_shift %||% rep(NA_real_, length(mass)))),
        sprintf("Shift %s [s]", mass)
      )
    )
  }

  geometry
}

# .dxf — peak lists are grouped per gas under CResultArray/CResultForGas in the
# "Results" CBlockData.
read_dxf_data_table <- function(json_path) {
  block_query <- "/CContiniousFlowBlockData/p/objects/CBlockData"
  block_idx <- find_json_block_idx_by_value(
    json_path,
    block_query,
    "Results",
    required = FALSE
  )
  if (is.na(block_idx)) {
    return(NULL)
  }
  base <- sprintf(
    "%s/%d/objects/CResultArray/p/objects/CResultForGas",
    block_query,
    block_idx
  )

  # collect the CGCPeakList pointer for each gas. With multiple gases the
  # CResultForGas entries are integer-indexed (CResultForGas/0, /1, ...); with a
  # single gas the entry is inlined directly under CResultForGas (no index), so
  # check that direct location first.
  has_peaklist <- function(ptr) {
    !json_missing(query_json(
      json_path,
      paste0(ptr, "/p/n_objects"),
      required = FALSE
    ))
  }
  single_ptr <- sprintf("%s/CGCPeakList", base)
  if (has_peaklist(single_ptr)) {
    peaklist_ptrs <- single_ptr
  } else {
    peaklist_ptrs <- character(0)
    g <- 0L
    repeat {
      ptr <- sprintf("%s/%d/CGCPeakList", base, g)
      if (!has_peaklist(ptr)) {
        break
      }
      peaklist_ptrs <- c(peaklist_ptrs, ptr)
      g <- g + 1L
    }
  }

  tables <- lapply(
    peaklist_ptrs,
    function(ptr) read_isodat_gc_peak_table(json_path, ptr)
  )
  out <- dplyr::bind_rows(tables)
  if (is.null(out) || nrow(out) == 0L) NULL else out
}

# .cf — single CGCPeakList in the "Data Block" CBlockData.
read_cf_data_table <- function(json_path) {
  block_idx <- find_json_block_idx_by_value(
    json_path,
    "/CBlockData",
    "Data Block",
    required = FALSE
  )
  if (is.na(block_idx)) {
    return(NULL)
  }
  read_isodat_gc_peak_table(
    json_path,
    sprintf(
      "/CBlockData/%d/objects/CBlockDataContext/p/objects/CGCPeakList",
      block_idx
    )
  )
}

# .caf — rows are CResultData entries grouped per gas under CResultDataList in
# the "Result Data List Block" CBlockData.
read_caf_data_table <- function(json_path) {
  block_query <- "/CBlockDataContext/p/objects/CBlockData"
  block_idx <- find_json_block_idx_by_value(
    json_path,
    block_query,
    "Result Data List Block",
    required = FALSE
  )
  if (is.na(block_idx)) {
    return(NULL)
  }
  base <- sprintf("%s/%d/objects/CResultDataList", block_query, block_idx)
  tables <- list()
  g <- 0L
  repeat {
    n_rows <- query_json(
      json_path,
      sprintf("%s/%d/p/n_objects", base, g),
      required = FALSE
    )
    if (json_missing(n_rows)) {
      break
    }
    rows <- purrr::map(seq_len(as.integer(n_rows)) - 1L, function(i) {
      species <- query_json(
        json_path,
        sprintf("%s/%d/p/objects/CResultData/%d/gas_name", base, g, i),
        required = FALSE
      )
      cells <- read_isodat_eval_cells(
        json_path,
        sprintf(
          "%s/%d/p/objects/CResultData/%d/CEvalDataItemListTransferPart/p/objects",
          base,
          g,
          i
        )
      )
      if (is.null(cells)) {
        return(NULL)
      }
      tibble::as_tibble_row(c(
        list(
          analysis = 1L,
          species = if (json_missing(species)) {
            NA_character_
          } else {
            as.character(species)
          }
        ),
        cells
      ))
    })
    tables[[length(tables) + 1L]] <- dplyr::bind_rows(rows)
    g <- g + 1L
  }
  out <- dplyr::bind_rows(tables)
  if (is.null(out) || nrow(out) == 0L) NULL else out
}

# .did — evaluated columns (CTwoDoublesArrayData) of cycle/value pairs, one column
# per computed quantity (e.g. "d 45CO2/44CO2"), in the "Evaluated Results"
# CBlockData. Pivoted to wide cycle x columns.
read_did_data_table <- function(json_path) {
  block_query <- "/CDualInletBlockData/p/objects/CBlockData"
  block_idx <- find_json_block_idx_by_value(
    json_path,
    block_query,
    "Evaluated Results",
    required = FALSE
  )
  if (is.na(block_idx)) {
    return(NULL)
  }
  base <- sprintf(
    "%s/%d/objects/CDualInletEvaluatedDataCollect/p/objects/CDualInletEvaluatedData",
    block_query,
    block_idx
  )
  out <- NULL
  i <- 0L
  repeat {
    name <- query_json(
      json_path,
      sprintf("%s/%d/p/p/v", base, i),
      required = FALSE
    )
    if (json_missing(name)) {
      break
    }
    x <- query_json(
      json_path,
      sprintf("%s/%d/p/objects/CTwoDoublesArrayData/x_data", base, i),
      required = FALSE
    )
    y <- query_json(
      json_path,
      sprintf("%s/%d/p/objects/CTwoDoublesArrayData/y_data", base, i),
      required = FALSE
    )
    if (!json_missing(x) && !json_missing(y)) {
      col <- tibble(
        analysis = 1L,
        cycle = as.integer(x) + 1L, # 0-based in the file
        !!trimws(as.character(name)) := as.numeric(y)
      )
      out <- if (is.null(out)) {
        col
      } else {
        dplyr::full_join(out, col, by = c("analysis", "cycle"))
      }
    }
    i <- i + 1L
  }
  if (is.null(out) || nrow(out) == 0L) {
    return(NULL)
  }
  dplyr::arrange(out, .data$cycle)
}

# file type specific readers ==========

## dxf =====================

# .dxf — continuous-flow file (CContiniousFlowBlockData root).
# Returns info (timestamp + seq line metadata), resistors (calibrated), traces.
read_dxf_json <- function(json_path) {
  # metadata: seq line info  + timestamp
  metadata <- json_path |>
    read_dxf_metadata() |>
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
  traces <- read_dxf_traces(json_path, gas_names$result) |>
    match_channels_to_masses(resistors$result) |>
    try_catch_cnds()

  # isodat computed data table
  vendor_data_table <- read_dxf_data_table(json_path) |> try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    metadata$conditions,
    gas_names$conditions,
    resistors$conditions,
    traces$conditions,
    vendor_data_table$conditions
  )

  # return value
  tibble(
    metadata = list(metadata$result),
    resistors = list(resistors$result),
    traces = list(traces$result),
    vendor_data_table = list(vendor_data_table$result),
    problems = list(problems)
  )
}

read_dxf_metadata <- function(json_path) {
  # The "Sequence Line Information" block is missing in some .dxf files
  # --> cathch issues and re-issue as warning
  seq_line <- json_path |>
    read_isodat_seq_line_info(
      "/CContiniousFlowBlockData/p/objects/CBlockData"
    ) |>
    try_catch_cnds(error_value = tibble::tibble(analysis = 1L))
  warn_cnds(seq_line$conditions, include_cnd_symbols = FALSE)

  seq_line$result |>
    dplyr::mutate(
      type = "cf",
      timestamp = read_isodat_timestamp(json_path),
      h3_factor = read_isodat_h3_factor(
        json_path,
        c(
          "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/p/objects/CGasConfiguration",
          "/CContiniousFlowBlockData/p/objects/CBlockData/5/objects/CMethod/p/objects/CNumericValue/CGasConfiguration"
        )
      ),
      .after = "analysis"
    )
}

# Read raw trace data from the "RawDataBlock" CBlockData of a .dxf JSON file.
# gas_names[i] is assigned to the i-th CRawData entry. Returns a long-format tibble:
# species (chr), channel (int), time.s (dbl), intensity.V (dbl).
read_dxf_traces <- function(json_path, gas_names) {
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
          dplyr::mutate(species = !!gas, .after = "analysis")
      }
    ) |>
    purrr::list_rbind()
}

## cf ===============

# .cf — continuous-flow file (CMethod root).
# Returns info (timestamp + seq line metadata), resistors (calibrated), traces.
read_cf_json <- function(json_path) {
  # metadata: seq line info  + timestamp
  metadata <- json_path |>
    read_cf_metadata() |>
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
  traces <- read_cf_traces(json_path, gas_names$result) |>
    match_channels_to_masses(resistors$result) |>
    try_catch_cnds()

  # isodat computed data table
  vendor_data_table <- read_cf_data_table(json_path) |> try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    metadata$conditions,
    gas_names$conditions,
    resistors$conditions,
    traces$conditions,
    vendor_data_table$conditions
  )

  # return value
  tibble(
    metadata = list(metadata$result),
    resistors = list(resistors$result),
    traces = list(traces$result),
    vendor_data_table = list(vendor_data_table$result),
    problems = list(problems)
  )
}

read_cf_metadata <- function(json_path) {
  json_path |>
    read_isodat_seq_line_grid(
      grid_ptr = "/CBlockData/1/objects/CSequenceLineInformationGridStorage/p/CGridCtrl/cells"
    ) |>
    dplyr::mutate(
      type = "cf",
      timestamp = read_isodat_timestamp(json_path),
      h3_factor = read_isodat_h3_factor(
        json_path,
        "/CMethod/p/objects/CGasConfiguration"
      ),
      .after = "analysis"
    )
}

# Read raw trace data from a .cf JSON file. Gas names must be supplied externally;
# the data blocks carry no gas identity. Tries three structural variants in order
# (multi-gas indexed, single-gas ScanStorage, single-gas CRawData).
read_cf_traces <- function(json_path, gas_names) {
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
          dplyr::mutate(species = !!gas, .after = "analysis")
      }
    ) |>
    purrr::list_rbind()
}

## did ===========

# .did — dual-inlet file (CDualInletBlockData root).
# Returns info (timestamp + seq line metadata), resistors (calibrated), cycles.
# CEvalIntegrationUnitHWInfoStore may be wrapped in CNumericValue (version-dependent).
read_did_json <- function(json_path) {
  # metadata: seq line info  + timestamp
  metadata <-
    json_path |>
    read_did_metadata() |>
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
    dplyr::mutate(species = gas_name$result, .after = "analysis") |>
    match_channels_to_masses(resistors$result) |>
    try_catch_cnds()

  # isodat computed data table
  vendor_data_table <- read_did_data_table(json_path) |> try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    metadata$conditions,
    gas_name$conditions,
    resistors$conditions,
    cycles$conditions,
    vendor_data_table$conditions
  )

  # return value
  tibble(
    metadata = list(metadata$result),
    resistors = list(resistors$result),
    cycles = list(cycles$result),
    vendor_data_table = list(vendor_data_table$result),
    problems = list(problems)
  )
}

read_did_metadata <- function(json_path) {
  json_path |>
    read_isodat_seq_line_info("/CDualInletBlockData/p/objects/CBlockData") |>
    dplyr::mutate(
      type = "di",
      timestamp = read_isodat_timestamp(json_path),
      h3_factor = read_isodat_h3_factor(
        json_path,
        "/CDualInletBlockData/p/objects/CMethod/p/objects/CGasConfiguration"
      ),
      .after = "analysis"
    )
}

## caf ===================

# .caf — dual-inlet file (CBlockDataContext root).
# Returns info (timestamp + seq line metadata), resistors (calibrated), cycles.
read_caf_json <- function(json_path) {
  # metadata: seq line info  + timestamp
  metadata <-
    json_path |>
    read_caf_metadata() |>
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
    dplyr::mutate(species = gas_name$result, .after = "analysis") |>
    match_channels_to_masses(resistors$result) |>
    try_catch_cnds()

  # isodat computed data table
  vendor_data_table <- read_caf_data_table(json_path) |> try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    metadata$conditions,
    gas_name$conditions,
    resistors$conditions,
    cycles$conditions,
    vendor_data_table$conditions
  )

  # return value
  tibble(
    metadata = list(metadata$result),
    resistors = list(resistors$result),
    cycles = list(cycles$result),
    vendor_data_table = list(vendor_data_table$result),
    problems = list(problems)
  )
}

read_caf_metadata <- function(json_path) {
  json_path |>
    read_isodat_seq_line_grid(
      "/CBlockDataContext/p/objects/CSequenceLineInformationGridStorage/p/CGridCtrl/cells"
    ) |>
    dplyr::mutate(
      type = "di",
      timestamp = read_isodat_timestamp(json_path),
      h3_factor = read_isodat_h3_factor(
        json_path,
        "/CBlockDataContext/p/objects/CMethod/p/objects/CGasConfiguration"
      ),
      .after = "analysis"
    )
}

## scn =====================

# .scn — scan file (CScanStorage root). No calibrated resistors; uses nominal values from CCupHardwarePart.
# Returns info (timestamp, type, x_units, comment), resistors (nominal), traces.
read_scn_json <- function(json_path) {
  # metadata: scan type, x unit, comment, timestamp
  metadata <- read_scn_metadata(json_path) |> try_catch_cnds()

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
  scans <- read_scn_scans(json_path) |>
    # add species since nominal raw data is not species specific
    dplyr::mutate(species = gas_name$result, .after = "analysis") |>
    match_channels_to_masses(nominal_resistors$result) |>
    try_catch_cnds()

  # problems
  problems <- dplyr::bind_rows(
    empty_cnds_tibble(),
    metadata$conditions,
    gas_name$conditions,
    nominal_resistors$conditions,
    scans$conditions
  )

  # return value
  tibble(
    metadata = list(metadata$result),
    resistors = list(nominal_resistors$result),
    scans = list(scans$result),
    problems = list(problems)
  )
}


# Detect the scan type from CScanStorage. Returns list(scan_type, x_unit, part).
# Tries CScaleHvScanPart ("high voltage"), CMagnetCurrentScanPart ("magnet current"),
# CClockScanPart ("time") in order; aborts if none is present.
read_scn_type <- function(json_path) {
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

# Read .scn file metadata from CScanStorage
read_scn_metadata <- function(json_path) {
  type_info <- read_scn_type(json_path)
  comment_raw <- json_path |>
    query_json("/CScanStorage/comment", required = FALSE)
  comment <- if (!json_missing(comment_raw) && nzchar(comment_raw)) {
    comment_raw
  } else {
    NA_character_
  }
  ts <- query_json(json_path, "/CScanStorage/timestamp_start")
  tibble(
    analysis = 1L, # always a single analysis
    type = "scan",
    timestamp = parse_iso8601_datetime(ts),
    scan_type = type_info$scan_type,
    x_units = type_info$x_unit,
    comment = comment
  )
}

# Read raw scan data from CScanStorage/CBinary. Converts x from raw steps to physical units
# (kV for high-voltage, steps for magnet current, s for time).
# Returns a long-format tibble: channel (int), x (dbl), intensity.V (dbl).
read_scn_scans <- function(json_path) {
  binary <- query_json(json_path, "/CScanStorage/CBinary")
  type_info <- read_scn_type(json_path)
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
