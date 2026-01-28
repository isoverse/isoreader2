# general class readers ======

read_CData_derived_object <- function(bfile) {
  info <- bfile |> read_CRuntimeClass(class = NULL) # read actual class header
  if (nrow(info) == 0L) {
    return(tibble::tibble())
  }

  cls <- info$class[[1]]
  fn <- paste0("read_", cls)

  if (!exists(fn, mode = "function", inherits = TRUE)) {
    bfile |>
      register_cnd(
        cli_abort(
          "no reader implemented for CData-derived class {.field {cls}}"
        ),
        pos = bfile$pos
      )
    return(info)
  }

  obj <- get(fn, mode = "function", inherits = TRUE)(bfile)

  info |>
    dplyr::select(-dplyr::any_of(names(obj))) |>
    dplyr::bind_cols(obj)
}

# CData chain ===================

# read CData object (complete)
read_CData <- function(bfile) {
  # fields
  data <-
    list(
      version = bfile |> read_schema_version("CData", 3),
      app_id = bfile |> read_binary_data("uint16"), # +0x04 (enum APP_ID returned by CData::GetOwner, often set to 0x2f = 47 in constructors)
      x34 = bfile |> read_binary_data("string"), # file name?
      value = bfile |> read_binary_data("string") # +0x38
    )

  # additional data for version >= 3
  if (!is.na(data$version) && data$version >= 3) {
    # +0x7c holds bit flags with the following meanings
    # +0x7c & 1 = CData::IsSystemOnly
    # +0x7c & 2 = CData::BackupFile
    # +0x7c & 4 = CData::IsDisabled
    # +0x7c & 8 = CData::IsAdvanced
    data$flags <- bfile |> read_binary_data("int") #0x7e
  }

  return(dplyr::as_tibble(data))
}


# read CBlockData object (complete BUT derived classes need to take care of the objects stored in the CBlock array)
read_CBlockData <- function(bfile) {
  # parent
  data <- list(
    pCData = bfile |> read_CData() |> list(),
    version = bfile |> read_schema_version("CBlockData", 2),
    n_objects = bfile |> read_binary_data("int") # +0x9C
  )

  # array of n (n_objects) CData-dervied child objects at +0x98
  # these are read with: class CObject object = CArchive::ReadObject(ar, &CData::classCData)
  # the child objects need to be loaded in the derived classes as CBlockData does not know their exact type
  return(tibble::as_tibble(data))
}

# read CBasicInterface object
read_CBasicInterface <- function(bfile) {
  data <- list(
    pCData = bfile |> read_CData() |> list()
  )
  return(tibble::as_tibble(data))
}

read_CFinniganInterface <- function(bfile) {
  data <- list(pCBasicInterface = bfile |> read_CBasicInterface() |> list())

  data$version <- read_schema_version(
    bfile,
    "CFinniganInterface",
    max_supported = 6
  )

  data <- bfile |> read_binary_data_list(data = data, c("param_0x9c" = "int"))

  if (!is.na(data$version) && data$version >= 3) {
    data <- bfile |>
      read_binary_data_list(
        data = data,
        c("param_0xa0" = "int", "chk_409_option1" = "int")
      )
  }

  if (!is.na(data$version) && data$version >= 5) {
    data <- bfile |>
      read_binary_data_list(data = data, c("chk_40a_master" = "int"))
  }

  if (!is.na(data$version) && data$version >= 6) {
    data <- bfile |>
      read_binary_data_list(data = data, c("chk_40b_dependent" = "int"))
  }

  dplyr::as_tibble(data)
}

read_CGpibInterface <- function(bfile) {
  data <- list(
    pCBasicInterface = bfile |> read_CBasicInterface() |> list()
  )

  # CGpibInterface schema version (writes 3)
  data$gpib_version <- read_schema_version(
    bfile,
    "CGpibInterface",
    max_supported = 3
  )

  # config bytes
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "gpib_cfg_byte1" = "uint8", # +0x9c
        "gpib_cfg_byte2" = "uint8" # +0x9d
      )
    )

  # added in schema version 3
  if (!is.na(data$gpib_version) && data$gpib_version >= 3) {
    data <- bfile |>
      read_binary_data_list(
        data = data,
        c("gpib_cfg_byte3" = "uint8") # +0x9e
      )
  }

  tibble::as_tibble(data)
}

# read CBasicScan object
read_CBasicScan <- function(bfile) {
  # parent
  data <- list(pCData = bfile |> read_CData() |> list())

  # CBasicScan schema version (writes 4)
  data$version <- read_schema_version(
    bfile,
    "CBasicScan",
    max_supported = 4
  )
  v <- data$version

  # local: read polymorphic MFC object (reads runtime class header, dispatches to read_<Class>)
  read_mfc_any <- function(bfile) {
    info <- bfile |> read_CRuntimeClass(class = NULL)
    if (nrow(info) == 0L) {
      return(tibble::tibble())
    }

    cls <- info$class[[1]]
    fn <- paste0("read_", cls)

    if (!exists(fn, mode = "function", inherits = TRUE)) {
      bfile |>
        register_cnd(
          cli_abort("no reader implemented for runtime class {.field {cls}}"),
          pos = bfile$pos
        )
      return(info)
    }

    obj <- get(fn, mode = "function", inherits = TRUE)(bfile)

    info |>
      dplyr::select(-dplyr::any_of(names(obj))) |>
      dplyr::bind_cols(obj)
  }

  # objects: X scan part, Y scan part, block data container
  data$pXScanPart <- (bfile |> read_mfc_any()) |> list()
  data$pYScanPart <- (bfile |> read_mfc_any()) |> list()
  data$pBlockData <- (bfile |> read_mfc_any()) |> list()

  # fields after objects
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "APP_ID_32" = "int", # uint32 in C++
        "flags_0xA0" = "int" # uint32 in C++
      )
    )

  # gated field (version >= 4)
  if (!is.na(v) && v >= 4) {
    data <- bfile |>
      read_binary_data_list(data = data, c("mode_0x94" = "int")) # uint32 in C++
  }

  dplyr::as_tibble(data)
}


# read CGasConfiguration object
read_CGasConfiguration <- function(bfile) {
  # parent
  data <- list(pCBlockData = bfile |> read_CBlockData() |> list())

  # CBlockData child objects (count stored in CBlockData)
  n <- data$pCBlockData[[1]]$n_objects

  # ReadObject(..., &CData::classCData) -> CData-derived objects
  if (!is.na(n) && n > 0) {
    kids <- vector("list", n)
    for (i in seq_len(n)) {
      kids[[i]] <- bfile |> read_CData_derived_object()
    }
    data$child_objects <- list(kids)
  } else {
    data$child_objects <- list(list())
  }

  # CGasConfiguration schema version (writes 3)
  data$version <- read_schema_version(
    bfile,
    "CGasConfiguration",
    max_supported = 3
  )
  v <- data$version

  # modified timestamp (only present for version >= 3)
  if (!is.na(v) && v >= 3) {
    data <- bfile |>
      read_binary_data_list(data = data, c("modified_time" = "int"))
  }

  dplyr::as_tibble(data)
}

# read CVisualisationData object
# read CVisualisationData object (covers full CSV including child objects)
read_CVisualisationData <- function(bfile) {
  # parent
  data <- list(pCBlockData = bfile |> read_CBlockData() |> list())

  # CBlockData child objects (count stored in CBlockData)
  n <- data$pCBlockData[[1]]$n_objects

  # ReadObject(..., &CData::classCData) -> CData-derived objects
  if (!is.na(n) && n > 0) {
    kids <- vector("list", n)
    for (i in seq_len(n)) {
      kids[[i]] <- bfile |> read_CData_derived_object()
    }
    data$child_objects <- list(kids)
  } else {
    data$child_objects <- list(list())
  }

  # CVisualisationData schema version (writes 8)
  data$version <- read_schema_version(
    bfile,
    "CVisualisationData",
    max_supported = 8
  )
  v <- data$version

  # fixed byte block (16 bytes)
  data$bytes_16 <- list(replicate(16, bfile |> read_binary_data("uint8")))

  # array of uint32 (10 items): block A
  data$blockA_u32_10 <- list(replicate(10, bfile |> read_binary_data("int")))

  # array of uint32 (10 items): block B
  data$blockB_u32_10 <- list(replicate(10, bfile |> read_binary_data("int")))

  # 3 strings (version >= 2)
  if (!is.na(v) && v >= 2) {
    data <- bfile |>
      read_binary_data_list(
        data = data,
        c("str_1" = "string", "str_2" = "string", "str_3" = "string")
      )
  }

  # gated options (keep in this order)
  if (!is.na(v) && v >= 3) {
    data <- bfile |> read_binary_data_list(data = data, c("opt_A_u32" = "int"))
  }
  if (!is.na(v) && v >= 4) {
    data <- bfile |> read_binary_data_list(data = data, c("opt_B_u32" = "int"))
  }
  if (!is.na(v) && v >= 5) {
    data <- bfile |>
      read_binary_data_list(data = data, c("preset_name" = "string"))
  }
  if (!is.na(v) && v >= 6) {
    data <- bfile |> read_binary_data_list(data = data, c("opt_C_u32" = "int"))
  }
  if (!is.na(v) && v >= 7) {
    data <- bfile |> read_binary_data_list(data = data, c("opt_D_u32" = "int"))
  }
  if (!is.na(v) && v >= 8) {
    data <- bfile |> read_binary_data_list(data = data, c("opt_E_u32" = "int"))
  }

  dplyr::as_tibble(data)
}
#===========================================================
# CGpibInterface
# Parent: CBasicInterface (which reads CData)
# After parent fields:
#   gpib_version (int, const 3 on write)
#   cfg_byte1 (uint8)
#   cfg_byte2 (uint8)
#   cfg_byte3 (uint8) only if gpib_version >= 3
#===========================================================

read_CGpibInterface <- function(bfile) {
  data <- list(
    pCBasicInterface = bfile |> read_CBasicInterface() |> list()
  )

  # CGpibInterface schema version (writes 3)
  data$gpib_version <- read_schema_version(
    bfile,
    "CGpibInterface",
    max_supported = 3
  )

  # config bytes
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "gpib_cfg_byte1" = "uint8", # +0x9c
        "gpib_cfg_byte2" = "uint8" # +0x9d
      )
    )

  # added in schema version 3
  if (!is.na(data$gpib_version) && data$gpib_version >= 3) {
    data <- bfile |>
      read_binary_data_list(
        data = data,
        c("gpib_cfg_byte3" = "uint8") # +0x9e
      )
  }

  tibble::as_tibble(data)
}

# CSimple chain ======

# read CSimple object (complete)
read_CSimple <- function(bfile) {
  # fields
  data <-
    list(
      version = bfile |> read_schema_version("CSimple", max_supported = 2),
      "x38" = bfile |> read_binary_data("string")
    )
  return(dplyr::as_tibble(data))
}

# read CBinary object (complete)
read_CBinary <- function(bfile, n_points, n_traces, read_data = TRUE) {
  # parent
  data <- list(
    pCSimple = bfile |> read_CSimple() |> list(),
    version = bfile |> read_schema_version("CBinary", max_supported = 2),
    n_bytes = bfile |> read_binary_data("int")
  )

  # data
  if (
    read_data && !is.na(n_points) && !is.na(n_traces) && !is.na(data$n_bytes)
  ) {
    # n_bytes = 4 * n_points + 8 * n_traces * n_points
    # safety check that this matches the provided n_points and n_traces
    # (can not uniquely identify the number of traces/points from n_bytes alone! there are multiple solutions)
    if (data$n_bytes != 4L * n_points + 8L * n_traces * n_points) {
      bfile |>
        register_cnd(
          cli_abort(
            "number of bytes ({data$n_bytes}) does not match what is expected based on the number of points ({n_points}) and traces ({n_traces})"
          )
        )
    }
    types <- c("float", rep("double", n_traces))
    data$voltages <- bfile |>
      read_binary_data_array(types, n = n_points) |>
      list()
  }

  return(dplyr::as_tibble(data))
}


# CPlotInfo / CPlotRange =========

# we do not have a decompiled code base for this
# --> inferred by manual inspection (complete)
# CPlotInfo and CPlotRange have separate serialization indices
# so need to be started with a fresh index (see use in
# read_CScanStorage)
read_CPlotInfo <- function(bfile) {
  # version
  data <- list(
    version = bfile |> read_schema_version("CPlotInfo", max_supported = 2)
  )

  # fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "?" = "int", # usually 0
        "?" = "int", # usually 1
        "?" = "int", # usually 1
        "?" = "int", # usually 1
        "?" = "float" # usually 10,000.00, is this the max accelerating voltage?
      )
    )

  # spacer sequence (NOT an empty string)
  spacer <- bfile |>
    read_binary_data(
      "raw",
      4L,
      expected = as.raw(c(0xff, 0xff, 0xff, 0x00)),
      block_if_unexpected = FALSE
    )

  # next fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "?" = "int", # usually 0
        "?" = "int", # usually 1
        "?" = "raw", # usually 0E - is this an object reference of some kind?
        "?" = "raw", # usually 00
        "font" = "string", # usually Arial
        "x_label" = "string", # without units!
        "y_label" = "string", # without units!
        "?" = "string" # usually Ratio
      )
    )

  # CTrace Info
  data$CTraceInfo <- bfile |> read_object("CTraceInfo") |> list()

  return(dplyr::as_tibble(data))
}

# we do not have decomplied code for this
# --> inferred by manual inspection (complete)
read_CTraceInfo <- function(bfile) {
  # no version serialized!
  # spacer sequence (NOT an empty string)
  spacer <- bfile |>
    read_binary_data(
      "raw",
      4L,
      expected = as.raw(c(0xff, 0xff, 0xff, 0x00)),
      block_if_unexpected = FALSE
    )

  # number of traces
  data <- list()
  data$n_traces <- bfile |> read_binary_data("uint8")

  # read CTraceInfoEntry records
  if (!is.na(data$n_traces) && data$n_traces > 0) {
    data$CTraceInfoEntry <-
      1:data$n_traces |>
      purrr::map(
        ~ read_object(bfile, "CTraceInfoEntry")
      ) |>
      dplyr::bind_rows() |>
      list()
  }

  # n_traces number again
  data$n_traces <- bfile |> read_binary_data("uint8")

  # read the trace labels (one for each trace)
  if (!is.na(data$n_traces) && data$n_traces > 0) {
    data$trace_labels <-
      1:data$n_traces |>
      purrr::map_chr(
        ~ bfile |> read_binary_data("string")
      ) |>
      list()
  }

  return(dplyr::as_tibble(data))
}

# we do not have decomplied code for this
# --> inferred by manual inspection (complete)
read_CTraceInfoEntry <- function(bfile) {
  # no version serialized!
  data <- bfile |>
    read_binary_data_list(
      c(
        "idx" = "uint8", # 0, 1, 2, etc.
        "?" = "raw", # usually FF
        "?" = "int", # variations of 08 00 00 00, 00 08 00 00, 00 00 08 00, etc.
        "?" = "int", # usually 0
        "?" = "int", # usually 0
        "?" = "int" # usually 1
      )
    )
  return(dplyr::as_tibble(data))
}

# we do not have decomplied code for this
# --> inferred by manual inspection (complete)
read_CPlotRange <- function(bfile, n_traces) {
  # no version serialized!
  data <- bfile |>
    read_binary_data_list(
      c(
        "xmin" = "float",
        "xmax" = "float",
        "ymin" = "double",
        "ymax" = "double"
      )
    )

  # spacer - this actually looks like a typical class reference (09 80)
  # but the fields after don't fit that pattern
  spacer <- bfile |> read_binary_data("raw", 2L)

  # fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "zoom_xmin" = "float",
        "zoom_xmax" = "float",
        "zoom_ymin" = "double",
        "zoom_ymax" = "double",
        "?" = "int",
        "?" = "int",
        # this seems to be just an inverted replication
        "zoom_xmin_rep" = "float",
        "zoom_xmax_rep" = "float",
        "zoom_ymin_rep" = "double",
        "zoom_ymax_rep" = "double",
        "xmin_rep" = "float",
        "xmax_rep" = "float",
        "ymin_rep" = "double",
        "ymax_rep" = "double"
      )
    )

  # read the trace labels (one for each trace)
  # this information is also in CTraceInfo
  if (!is.na(n_traces) && n_traces > 0) {
    data$trace_labels <-
      1:n_traces |>
      purrr::map_chr(
        ~ bfile |> read_binary_data("string")
      ) |>
      list()
  }
  return(dplyr::as_tibble(data))
}


# molecule class readers =========

read_CMolecule <- function(bfile) {
  data <- list(pCData = bfile |> read_CData() |> list())

  data$version <- read_schema_version(bfile, "CMolecule", max_supported = 0)

  data <- bfile |> read_binary_data_list(data = data, c("formula" = "string"))

  dplyr::as_tibble(data)
}


# optional fallbacks (only used if your package does not already define these)
if (!exists("cli_warn", mode = "function")) {
  cli_warn <- function(text) {
    rlang::warning_cnd(message = cli::format_inline(text))
  }
}
if (!exists("cli_abort", mode = "function")) {
  cli_abort <- function(text) {
    rlang::error_cnd(message = cli::format_inline(text))
  }
}


#===========================================================
# CHardwarePart (parent reader)
#===========================================================
read_CHardwarePart <- function(bfile) {
  # parent + version
  data <- list(
    pCBasicInterface = bfile |> read_CBasicInterface() |> list(),
    version = bfile |> read_schema_version("CHardwarePart", max_supported = 10)
  )
  hv <- data$version

  # member object (likely derived from CBasicInterface)
  data$device_interface <- bfile |>
    read_interface_object() |>
    list()

  # hasGasConfPart flag + expected object
  hasGasConfPart <- bfile |> read_binary_data("int")
  data$hasGasConfPart <- hasGasConfPart
  if (!is.na(hasGasConfPart) && hasGasConfPart > 0) {
    if (!exists("read_CGasConfPart", mode = "function", inherits = TRUE)) {
      bfile |>
        register_cnd(
          cli_abort(
            "non-zero {.field hasGasConfPart} but read_CGasConfPart() is not implemented yet"
          ),
          pos = bfile$pos
        )
      return(dplyr::as_tibble(data))
    }
    data$gas_conf_part <- bfile |>
      read_object("CGasConfPart", read_CGasConfPart) |>
      list()
  }

  # hasMethodPart flag + expected object
  hasMethodPart <- bfile |> read_binary_data("int")
  data$hasMethodPart <- hasMethodPart
  if (!is.na(hasMethodPart) && hasMethodPart > 0) {
    if (!exists("read_CMethodPart", mode = "function", inherits = TRUE)) {
      bfile |>
        register_cnd(
          cli_abort(
            "non-zero {.field hasMethodPart} but read_CMethodPart() is not implemented yet"
          ),
          pos = bfile$pos
        )
      return(dplyr::as_tibble(data))
    }
    data$method_part <- bfile |>
      read_object("CMethodPart", read_CMethodPart) |>
      list()
  }

  # hasExtraData flag + expected base (CData)
  hasExtraData <- bfile |> read_binary_data("int")
  data$hasExtraData <- hasExtraData
  if (!is.na(hasExtraData) && hasExtraData > 0) {
    data$extra_data <- bfile |>
      read_object("CData", read_CData) |>
      list()
  }

  # checkboxes (version >= 3)
  if (!is.na(hv) && hv >= 3) {
    data <- bfile |>
      read_binary_data_list(
        data = data,
        c(
          "chk_404" = "int",
          "chk_405" = "int",
          "chk_406" = "int",
          "chk_407" = "int"
        )
      )
  }

  # visualisation section (version >= 7)
  if (!is.na(hv) && hv >= 7) {
    if (
      !exists("read_CVisualisationData", mode = "function", inherits = TRUE)
    ) {
      bfile |>
        register_cnd(
          cli_abort(
            "version >= 7 but read_CVisualisationData() is not implemented yet"
          ),
          pos = bfile$pos
        )
      return(dplyr::as_tibble(data))
    }
    data$visualisation_data <- bfile |>
      read_object("CVisualisationData", read_CVisualisationData) |>
      list()

    # 8 bytes (uint64 or double). Keep uint64 for now.
    data$edit_mode_visualisation_value <- bfile |> read_binary_data("double")

    data <- bfile |>
      read_binary_data_list(
        data = data,
        c("multi_select_visualisation" = "int")
      )
  }

  # CStringArray blocks (version >= 9)
  safe_count <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) {
      return(0L)
    }
    if (!is.numeric(x)) {
      return(0L)
    }
    x <- as.integer(x)
    if (is.na(x) || x < 0) {
      return(0L)
    }
    x
  }

  if (!is.na(hv) && hv >= 9) {
    data$set_hwparts_count <- bfile |> read_binary_data("int")
    n1 <- safe_count(data$set_hwparts_count)
    data$set_hwparts_strings <- list(lapply(
      seq_len(n1),
      function(i) bfile |> read_binary_data("string")
    ))

    data$get_hwparts_count <- bfile |> read_binary_data("int")
    n2 <- safe_count(data$get_hwparts_count)
    data$get_hwparts_strings <- list(lapply(
      seq_len(n2),
      function(i) bfile |> read_binary_data("string")
    ))
  }

  # text field (version >= 10)
  if (!is.na(hv) && hv >= 10) {
    data$hardware_text <- bfile |> read_binary_data("string")
  }

  dplyr::as_tibble(data)
}


#===========================================================
# CChannelHardwarePart
# Parent: CHardwarePart -> CBasicInterface -> CData
# After CHardwarePart:
#   channel_version (int, writes 2)
#   channel_param_1 (int)
#   channel_param_2 (int)
#===========================================================

read_CChannelHardwarePart <- function(bfile) {
  data <- list(
    pCHardwarePart = bfile |> read_CHardwarePart() |> list()
  )

  data$channel_version <- read_schema_version(
    bfile,
    "CChannelHardwarePart",
    max_supported = 2
  )

  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "channel_param_1" = "int",
        "channel_param_2" = "int"
      )
    )

  dplyr::as_tibble(data)
}


#===========================================================
# CScaleHardwarePart (parent reader, used by derived classes)
# Parent: CHardwarePart -> CBasicInterface -> CData
# After CHardwarePart:
#   scale_version (int, writes 12)
#   unit_string (string)
#   min_value (int)
#   max_value (int)
#   then several gated 4-byte fields (use "int" since uint32 is not supported)
#   then gated strings
#   then gated doubles (8 bytes) for v>=12
#===========================================================
read_CScaleHardwarePart <- function(bfile) {
  data <- list(
    pCHardwarePart = bfile |> read_CHardwarePart() |> list()
  )

  data$scale_version <- read_schema_version(
    bfile,
    "CScaleHardwarePart",
    max_supported = 12
  )
  sv <- data$scale_version

  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "unit_string" = "string",
        "min_value" = "int",
        "max_value" = "int"
      )
    )

  # v>=4 (uint32 in notes, but read_binary_data supports only "int" for 4 bytes)
  if (!is.na(sv) && sv >= 4) {
    data$fmt_param_1 <- bfile |> read_binary_data("int")
  }

  # v>=5
  if (!is.na(sv) && sv >= 5) {
    data$fmt_param_2 <- bfile |> read_binary_data("int")
    data$cfg_flag_1 <- bfile |> read_binary_data("int")
    data$cfg_val_1 <- bfile |> read_binary_data("int")
    data$cfg_val_2 <- bfile |> read_binary_data("int")
    data$enable_flag <- bfile |> read_binary_data("int")
  }

  # v>=6
  if (!is.na(sv) && sv >= 6) {
    data$str_param_1 <- bfile |> read_binary_data("string")
    data$rubber_cfg_1 <- bfile |> read_binary_data("int")
    data$rubber_cfg_2 <- bfile |> read_binary_data("int")
  }

  # v>=7
  if (!is.na(sv) && sv >= 7) {
    data$rubber_digits <- bfile |> read_binary_data("int")
  }

  # v>=8
  if (!is.na(sv) && sv >= 8) {
    data$str_param_2 <- bfile |> read_binary_data("string")
    data$action_flag <- bfile |> read_binary_data("int")
  }

  # v>=9
  if (!is.na(sv) && sv >= 9) {
    data$rubber_vis_1 <- bfile |> read_binary_data("int")
    data$rubber_vis_2 <- bfile |> read_binary_data("int")
  }

  # v>=10
  if (!is.na(sv) && sv >= 10) {
    data$rubber_label <- bfile |> read_binary_data("string")
  }

  # v>=11
  if (!is.na(sv) && sv >= 11) {
    data$rubber_cfg_3 <- bfile |> read_binary_data("int")
    data$rubber_cfg_4 <- bfile |> read_binary_data("int")
  }

  # v>=12
  if (!is.na(sv) && sv >= 12) {
    data$lower_bound <- bfile |> read_binary_data("double")
    data$upper_bound <- bfile |> read_binary_data("double")
  }

  dplyr::as_tibble(data)
}

#===========================================================
# CClockHardwarePart (derived from CScaleHardwarePart)
# After parent:
#   clock_version (int, writes 2)
#   clock_param (int)
#===========================================================
read_CClockHardwarePart <- function(bfile) {
  data <- list(
    pCScaleHardwarePart = bfile |> read_CScaleHardwarePart() |> list()
  )

  data$clock_version <- read_schema_version(
    bfile,
    "CClockHardwarePart",
    max_supported = 2
  )

  data <- bfile |>
    read_binary_data_list(
      data = data,
      c("clock_param" = "int")
    )

  dplyr::as_tibble(data)
}

#===========================================================
# CDacHardwarePart (derived from CScaleHardwarePart)
# After parent:
#   dac_version (int, writes 3)
#   4 uint8 config bytes
#   (v>=3) dac_device_name (string)
#===========================================================
read_CDacHardwarePart <- function(bfile) {
  data <- list(
    pCScaleHardwarePart = bfile |> read_CScaleHardwarePart() |> list()
  )

  data$dac_version <- read_schema_version(
    bfile,
    "CDacHardwarePart",
    max_supported = 3
  )
  dv <- data$dac_version

  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "dac_cfg_1" = "uint8",
        "dac_cfg_2" = "uint8",
        "dac_cfg_4" = "uint8",
        "dac_cfg_3" = "uint8"
      )
    )

  if (!is.na(dv) && dv >= 3) {
    data$dac_device_name <- bfile |> read_binary_data("string")
  }

  dplyr::as_tibble(data)
}


#===========================================================
# CMagnetCurrentHardwarePart
# Parent: CDacHardwarePart
# After parent:
#   param_A (uint32 in notes) -> read as "int" (4 bytes)
#   param_B (uint32 in notes) -> read as "int" (4 bytes)
#===========================================================
read_CMagnetCurrentHardwarePart <- function(bfile) {
  data <- list(
    pCDacHardwarePart = bfile |> read_CDacHardwarePart() |> list()
  )

  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "param_A" = "int",
        "param_B" = "int"
      )
    )

  dplyr::as_tibble(data)
}

#===========================================================
# CScaleHvHardwarePart
# Parent: CDacHardwarePart
# After parent:
#   hv_version (int, store writes 3; load returns early if <3)
#   (v>=3) hv_scale_value (double, 8 bytes)
#===========================================================
read_CScaleHvHardwarePart <- function(bfile) {
  data <- list(
    pCDacHardwarePart = bfile |> read_CDacHardwarePart() |> list()
  )

  v <- bfile |> read_schema_version("CScaleHvHardwarePart", max_supported = 3)
  data$hv_version <- v

  if (!is.na(v) && v >= 3) {
    data$hv_scale_value <- bfile |> read_binary_data("double")
  }

  dplyr::as_tibble(data)
}
