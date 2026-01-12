# general class readers ======

# read CData object (complete)
read_CData <- function(bfile) {
  # fields
  data <- bfile |>
    read_binary_data_list(
      c(
        "version" = "int", # const
        "APP_ID" = "uint16", # +0x04 (enum APP_ID returned by CData::GetOwner, often set to 0x2f = 47 in constructors)
        "?" = "string", # +0x34 # file name?
        "value" = "string" # +0x38
      )
    )

  # additional data for version >= 3
  if (!is.na(data$version) && data$version >= 3) {
    # +0x7c holds bit flags with the following meanings
    # +0x7c & 1 = CData::IsSystemOnly
    # +0x7c & 2 = CData::BackupFile
    # +0x7c & 4 = CData::IsDisabled
    # +0x7c & 8 = CData::IsAdvanced
    data <- bfile |>
      read_binary_data_list(data = data, c("flags" = "int"))
  }

  return(dplyr::as_tibble(data))
}


# read CBlockData object (complete BUT derived classes need to take care of the objects stored in the CBlock array)
read_CBlockData <- function(bfile) {
  # parent
  data <- list(
    pCData = bfile |> read_CData() |> list()
  )

  # fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "version" = "int", # const
        "n_objects" = "int" # +0x9C
      )
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


# scan class readers =========

# read CScanStorage object
read_CScanStorage <- function(bfile) {
  # parent

  data <- list(
    pCBlockData = bfile |> read_CBlockData() |> list()
  )

  # check on n_objects stored in CBlockData
  n_objects <- data$pCBlockData[[1]]$n_objects

  # CBlockData should NOT have any child objects for CScanStorage
  if (!is.na(n_objects) && n_objects > 0) {
    bfile |>
      register_cnd(
        cli_stop(
          "unexpectedly encountered {n_objects} CBlockData child objects in CScanStorage"
        ),
        pos = bfile$pos
      )
  }

  data$version <- bfile |> read_binary_data("int") # const
  # version check
  if (!is.na(data$version) && data$version < 4) {
    bfile |>
      register_cnd(
        cli_warn(
          "untested processing of 4 CString objects in CScanStorage version < 4"
        ),
        pos = bfile$pos
      )
    # these are discarded (just read for backwards compatibility)
    bfile |> read_binary_data("string")
    bfile |> read_binary_data("string")
    bfile |> read_binary_data("string")
    bfile |> read_binary_data("string")
  }
  # fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "comment" = "string", # +0xf4
        "n_points" = "int", # +0x168,
        "n_traces" = "int" # +0x16c
      )
    )
  # expect next object to be CBinary
  bfile |> read_CRuntimeClass("CBinary", advance = FALSE)
  return(dplyr::as_tibble(data))
}


<<<<<<< HEAD
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
      kids[[i]] <- bfile |> read_object("CData", read_CData)
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
=======
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
>>>>>>> added CHardwarePart and CChannelHardwarePart
}


# molecule class readers =========
read_CMolecule <- function(bfile) {
  data <- list(pCData = bfile |> read_CData() |> list())

  data$version <- read_schema_version(bfile, "CMolecule", max_supported = 0)

  data <- bfile |> read_binary_data_list(data = data, c("formula" = "string"))

  dplyr::as_tibble(data)
}
<<<<<<< HEAD
=======


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
    read_object("CBasicInterface", read_CBasicInterface) |>
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
  if (!is.na(hv) && hv >= 9) {
    data$set_hwparts_count <- bfile |> read_binary_data("int")
    data$set_hwparts_strings <- list(lapply(
      seq_len(max(0, data$set_hwparts_count)),
      function(i) bfile |> read_binary_data("string")
    ))

    data$get_hwparts_count <- bfile |> read_binary_data("int")
    data$get_hwparts_strings <- list(lapply(
      seq_len(max(0, data$get_hwparts_count)),
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
>>>>>>> added CHardwarePart and CChannelHardwarePart
