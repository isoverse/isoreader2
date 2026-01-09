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
        "file_name" = "string" # +0x38
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

# read CFinniganInterface object (complete)
read_CFinniganInterface <- function(bfile) {
  # parent
  data <- list(
    pCBasicInterface = bfile |> read_CBasicInterface() |> list()
  )

  # CFinniganInterface schema version (const on write, local on read)
  data$version <- bfile |> read_binary_data("int")

  # sanity check (current writers use 6)
  if (!is.na(data$version) && data$version > 6) {
    bfile |>
      register_cnd(
        cli_warn(
          "CFinniganInterface version {data$version} is newer than expected (6). Parser may be incomplete."
        ),
        pos = bfile$pos
      )
  }

  # field #1 (always present) +0x9C, DDX_Text control 0x403
  data <- bfile |> read_binary_data_list(data = data, c("param_0x9c" = "int"))

  # version >= 3 fields
  if (!is.na(data$version) && data$version >= 3) {
    data <- bfile |>
      read_binary_data_list(
        data = data,
        c(
          "param_0xa0" = "int", # +0xA0, DDX_Text control 0x408
          "opt_0xa4" = "int" # +0xA4, DDX_Check control 0x409 (stored as int32)
        )
      )
  }

  # version >= 5 fields
  if (!is.na(data$version) && data$version >= 5) {
    data <- bfile |>
      read_binary_data_list(
        data = data,
        c(
          "opt_0xa8" = "int" # +0xA8, DDX_Check control 0x40A
        )
      )
  }

  # version >= 6 fields
  if (!is.na(data$version) && data$version >= 6) {
    data <- bfile |>
      read_binary_data_list(
        data = data,
        c(
          "opt_0xac" = "int" # +0xAC, DDX_Check control 0x40B
        )
      )
  }

  return(dplyr::as_tibble(data))
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
