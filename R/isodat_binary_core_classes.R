# CData chain ===================

# read CData object (complete)
read_CData <- function(bfile) {
  # fields
  data <-
    list(
      version = bfile |> read_schema_version("CData", 3),
      app_id = bfile |> read_binary_data("uint16"), # +0x04 (enum APP_ID returned by CData::GetOwner, often set to 0x2f = 47 in constructors)
      label = bfile |> read_binary_data("string"), # +0x34
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

# read CData::CCalibrationPoint (complete)
read_CCalibrationPoint <- function(bfile) {
  # fields
  data <-
    list(
      pCData = bfile |> read_CData() |> list(),
      version = bfile |> read_schema_version("CCalibrationPoint", 3),
      x94 = bfile |> read_binary_data("int"),
      x98 = bfile |> read_binary_data("double")
    )

  # version gated fields
  if (!is.na(data$version) && data$version >= 3) {
    data$xA0 <- bfile |> read_binary_data("double")
    data$xA8 <- bfile |> read_binary_data("double")
  }

  return(dplyr::as_tibble(data))
}

# read CData::CBasicScan object (complete)
read_CBasicScan <- function(bfile) {
  # parent and version
  data <- list(
    pCData = bfile |> read_CData() |> list(),
    version = bfile |> read_schema_version("CBasicScan", max_supported = 4)
  )

  # CScanParts
  data$CScanPart1 <- bfile |> read_object(pattern = "ScanPart") |> list()
  data$CScanPart2 <- bfile |> read_object(pattern = "ScanPart") |> list()

  # CBlockData
  data$CBlockData <- bfile |> read_object("CBlockData") |> list()

  # check if there are CData derived child objects
  if (
    !is.na(data$CBlockData[[1]]$n_objects) &&
      data$CBlockData[[1]]$n_objects > 0
  ) {
    bfile |>
      register_cnd(cli_abort(
        "unexpectedly encountered {data$CBlockData[[1]]$n_objects} CData objects - not yet implemented"
      ))
  }

  # fields
  data$x04 <- bfile |> read_binary_data("int")
  data$xA4 <- bfile |> read_binary_data("int") # might be a flag

  # version gated fields
  if (!is.na(data$version) && data$version >= 4) {
    data$x94 <- bfile |> read_binary_data("int") # defaults to 1
  }

  return(dplyr::as_tibble(data))
}

# read CData::CMolecule (complete)
read_CMolecule <- function(bfile) {
  # parent and version
  data <- list(
    pCData = bfile |> read_CData() |> list(),
    version = bfile |> read_schema_version("CMolecule", max_supported = 1)
  )
  # fields
  data$molecule <- bfile |> read_binary_data("string")
  dplyr::as_tibble(data)
}

# CData::CBlockData chain ====

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


# read CCalibration object (complete)
read_CCalibration <- function(bfile) {
  # parent
  data <- list(
    pCBlockData = bfile |> read_CBlockData() |> list()
  )
  # read CBlockData objects
  if (!is.na(data$pCBlockData[[1]]$n_objects)) {
    data$CCalibrationPoint <-
      1:data$pCBlockData[[1]]$n_objects |>
      purrr::map(
        ~ read_object(bfile, "CCalibrationPoint")
      ) |>
      dplyr::bind_rows() |>
      list()
  }
  # version
  data$version <- bfile |> read_schema_version("CCalibration", 5)
  # fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "xA8" = "uint8",
        "xAC" = "string",
        "xB0" = "timestamp"
      )
    )

  # legacy byte in lower version
  if (!is.na(data$version) && data$version < 5) {
    bfile |> read_binary_data("double")
  }

  # fields
  data$xBC <- bfile |> read_binary_data("int")

  # another byte
  if (!is.na(data$version) && data$version >= 3) {
    data$xC0 <- bfile |> read_binary_data("uint8")
  }

  # splines
  if (!is.na(data$version) && data$version >= 4) {
    continue <- TRUE
    i <- 0
    splines <- tibble()
    while (continue) {
      i <- i + 1
      n <- rep(NA_integer_, 8)
      x <- rep(list(list()), 8)
      for (idx in 1:8) {
        n[idx] <- bfile |> read_binary_data("uint16")
        x[idx] <- bfile |> read_binary_data("double", n[idx]) |> list()
      }
      continue <- bfile |> read_binary_data("bool")
      splines <- splines |> dplyr::bind_rows(tibble(spline = i, n = n, x = x))
    }
    data$splines <- splines |> list()
  }

  return(tibble::as_tibble(data))
}

# read CBockData::CVisualisationData (complete)
read_CVisualisationData <- function(bfile) {
  # parent
  data <- list(
    pCBlockData = bfile |> read_CBlockData() |> list()
  )

  # check if there are CData derived child objects
  if (
    !is.na(data$pCBlockData[[1]]$n_objects) &&
      data$pCBlockData[[1]]$n_objects > 0
  ) {
    bfile |>
      register_cnd(cli_abort(
        "unexpectedly encountered {data$pCBlockData[[1]]$n_objects} CData objects - not yet implemented"
      ))
  }

  # CVisualisationData schema version (writes 8)
  data$version <- bfile |>
    read_schema_version("CVisualisationData", max_supported = 8)

  # double arrays (unknown function)
  data$xA8 <- bfile |> read_binary_data("int", 4) |> list()
  data$xB8 <- bfile |> read_binary_data("int", 10) |> list()
  data$xE0 <- bfile |> read_binary_data("int", 10) |> list()

  # version gated visulization options
  if (!is.na(data$version) && data$version >= 2) {
    data <- bfile |>
      read_binary_data_list(
        data = data,
        c(
          "font" = "string",
          "x10C" = "string",
          "x110" = "string"
        )
      )

    # version 3
    if (!is.na(data$version) && data$version >= 3) {
      data$x120 <- bfile |> read_binary_data("int")

      # version 4
      if (!is.na(data$version) && data$version >= 4) {
        data$x124 <- bfile |> read_binary_data("int")

        # version 5
        if (!is.na(data$version) && data$version >= 5) {
          data$x148 <- bfile |> read_binary_data("string")

          # version 6
          if (!is.na(data$version) && data$version >= 6) {
            data$x11C <- bfile |> read_binary_data("int")

            # version 7
            if (!is.na(data$version) && data$version >= 7) {
              data$x128 <- bfile |> read_binary_data("int")

              # version 8
              if (!is.na(data$version) && data$version >= 8) {
                data$x12C <- bfile |> read_binary_data("int")
              }
            }
          }
        }
      }
    }
  }

  return(dplyr::as_tibble(data))
}

# read CBockData::CGasConfiguration object (complete)
read_CGasConfiguration <- function(bfile) {
  # parent
  data <- list(pCBlockData = bfile |> read_CBlockData() |> list())

  # CBlockData child objects (count stored in CBlockData)
  n <- data$pCBlockData[[1]]$n_objects

  # check if there are CData derived child objects
  if (
    !is.na(data$pCBlockData[[1]]$n_objects) &&
      data$pCBlockData[[1]]$n_objects > 0
  ) {
    cdata_objects <- list()
    for (i in 1:data$pCBlockData[[1]]$n_objects) {
      obj <- bfile |> read_object(pattern = "C")
      if (!is.na(obj$obj_idx)) {
        cdata_objects <- c(
          cdata_objects,
          dplyr::tibble(list(obj)) |> set_names(obj$class) |> list()
        )
      }
    }
    data$CData <- cdata_objects |>
      dplyr::bind_cols(.name_repair = "unique_quiet") |>
      list()
  }

  # version
  data$version <- bfile |>
    read_schema_version("CGasConfiguration", max_supported = 3)

  # version gated fields
  if (!is.na(data$version) && data$version >= 3) {
    data$timestamp <- bfile |>
      read_binary_data("timestamp")
  }

  return(dplyr::as_tibble(data))
}


# CData::CBasicInterface chain =====

# read CData::CBasicInterface object (same as read CData, complete)
read_CBasicInterface <- read_CData

# read CData::CBasicInterface::CFinniganInterface object (complete)
read_CFinniganInterface <- function(bfile) {
  # parent and version
  data <- list(
    pCBasicInterface = bfile |> read_CBasicInterface() |> list(),
    version = bfile |>
      read_schema_version("CFinniganInterface", max_supported = 6),
    x9C = bfile |> read_binary_data("int") # unknown setting
  )

  # version gated fields
  if (!is.na(data$version) && data$version >= 3) {
    data$xA0 = bfile |> read_binary_data("int") # unknown setting
    data$xA4 = bfile |> read_binary_data("bool") # checkbox

    # additional checkboxs for higher versions
    if (!is.na(data$version) && data$version >= 5) {
      data$xA8 = bfile |> read_binary_data("bool") # checkbox
    }

    if (!is.na(data$version) && data$version >= 6) {
      data$xAC = bfile |> read_binary_data("bool") # checkbox
    }
  }
  dplyr::as_tibble(data)
}

# read CData::CBasicInterface::CGpibInterface (complete)
read_CGpibInterface <- function(bfile) {
  # parent and version
  data <- list(
    pCBasicInterface = bfile |> read_CBasicInterface() |> list(),
    version = bfile |>
      read_schema_version("CGpibInterface", max_supported = 3)
  )

  # fields
  data$x9C <- bfile |> read_binary_data("uint8")
  data$x9D <- bfile |> read_binary_data("uint8")

  # added in schema version 3
  if (!is.na(data$version) && data$version >= 3) {
    data$x9E <- bfile |> read_binary_data("uint8")
  }

  return(tibble::as_tibble(data))
}

# read CData:CBasicInterface::CTransferPart (complete)
read_CTransferPart <- function(bfile) {
  # parent and version
  data <- list(
    pCBasicInterface = bfile |> read_CBasicInterface() |> list(),
    version = bfile |>
      read_schema_version("CTransferPart", max_supported = 2)
  )
  # fields
  data$x9C <- bfile |> read_binary_data("int")
  data$xA0 <- bfile |> read_binary_data("int")
  return(tibble::as_tibble(data))
}

# read CData:CBasicInterface::CTransferPart::CAdcTransferPart (complete)
read_CAdcTransferPart <- function(bfile) {
  # parent and version
  data <- list(
    pCTransferPart = bfile |> read_CTransferPart() |> list(),
    version = bfile |>
      read_schema_version("CAdcTransferPart", max_supported = 2)
  )
  # fields
  data$raw_value <- bfile |> read_binary_data("int") # 0xA4
  return(tibble::as_tibble(data))
}

# same implementations as CAdcTransferPart
read_CDacTransferPart <- read_CAdcTransferPart
read_CDioTransferPart <- read_CAdcTransferPart
read_CBasicHvTransferPart <- read_CDacTransferPart
read_CCalculatingDacTransferPart <- read_CBasicHvTransferPart
read_CBasicHvTransferPart <- read_CDacTransferPart
read_CScaleHvTransferPart <- read_CBasicHvTransferPart

# read CData:CBasicInterface::CTransferPart::CDacTransferPart::CMagnetCurrentTransferPart (complete)
read_CMagnetCurrentTransferPart <- function(bfile) {
  # parent
  data <- list(
    pCDacTransferPart = bfile |> read_CDacTransferPart() |> list()
  )
  # fields
  data$xA8 <- bfile |> read_binary_data("bool")
  return(tibble::as_tibble(data))
}

# CData::CBasicInterface::CGasConfPart chain =========

# read CData::CBasicInterface::CGasConfPart object (same as CData/CInterface, complete)
read_CGasConfPart <- read_CBasicInterface

# read CData::CBasicInterface::CGasConfPart::CIntegrationUnitGasConfPart (complete)
read_CIntegrationUnitGasConfPart <- function(bfile) {
  # parent and version
  data <- list(
    pCGasConfPart = bfile |> read_CGasConfPart() |> list(),
    version = bfile |>
      read_schema_version("CIntegrationUnitGasConfPart", max_supported = 2)
  )
  data$n_configs <- bfile |> read_binary_data("uint8") # 0xA4
  if (!is.na(data$n_configs) && data$n_configs > 0) {
    data$CChannelGasConfPart <- 1:data$n_configs |>
      purrr::map(
        ~ read_object(bfile, "CChannelGasConfPart")
      ) |>
      dplyr::bind_rows() |>
      list()
  }
  dplyr::as_tibble(data)
}

# read CData::CBasicInterface::CGasConfPart::CChannelGasConfPart (complete)
read_CChannelGasConfPart <- function(bfile) {
  # parent and version
  data <- list(
    pCGasConfPart = bfile |> read_CGasConfPart() |> list(),
    version = bfile |>
      read_schema_version("CChannelGasConfPart", max_supported = 4)
  )
  # fields
  data$cup <- bfile |> read_binary_data("uint8")
  data$mass <- bfile |> read_binary_data("double")
  data$xA8 <- bfile |> read_binary_data("double")

  # version gated fields
  if (!is.na(data$version) && data$version >= 3) {
    data$xB0 <- bfile |> read_binary_data("bool") # some sort of checkbox

    # version 4
    if (!is.na(data$version) && data$version >= 4) {
      data$xB4 <- bfile |> read_binary_data("bool") # some sort of checkbox
      data$xB8 <- bfile |> read_binary_data("double")
    }
  }

  dplyr::as_tibble(data)
}

# CData::CBasicInterface::CScanPart chain =============

# read CScanPart object (complete)
read_CScanPart <- function(bfile) {
  # parent an version
  data <- list(
    pCBasicInterface = bfile |> read_CBasicInterface() |> list(),
    version = bfile |> read_schema_version("CScanPart", max_supported = 3)
  )

  # CHardwarePart
  data$CHardwarePart <- bfile |>
    read_object(pattern = "HardwarePart") |>
    list()

  # other fields
  data$xA0 <- bfile |> read_binary_data("int")
  data$xA4 <- bfile |> read_binary_data("int")
  data$xB0 <- bfile |> read_binary_data("int")

  return(dplyr::as_tibble(data))
}


# read CScanPart::CClockScanPart (complete)
read_CClockScanPart <- function(bfile) {
  # parent and version
  data <- list(
    pCScanPart = bfile |> read_CScanPart() |> list(),
    version = bfile |> read_schema_version("CClockScanPart", max_supported = 2)
  )
  # other fields
  data$scan_time <- bfile |> read_binary_data("int") # almost certain
  return(dplyr::as_tibble(data))
}

# read CScanPart::CScaleHvScanPart (complete)
read_CScaleHvScanPart <- function(bfile) {
  # parent and version
  data <- list(
    pCScanPart = bfile |> read_CScanPart() |> list(),
    version = bfile |>
      read_schema_version("CScaleHvScanPart", max_supported = 2)
  )
  # other fields
  data$start <- bfile |> read_binary_data("int")
  data$stop <- bfile |> read_binary_data("int")
  data$step <- bfile |> read_binary_data("int")
  data$delay <- bfile |> read_binary_data("int") # not 100% sure this is correct
  return(dplyr::as_tibble(data))
}

# read CScanPart::CMagnetCurrentScanPart (complete)
read_CMagnetCurrentScanPart <- function(bfile) {
  # parent and version
  data <- list(
    pCScanPart = bfile |> read_CScanPart() |> list(),
    version = bfile |>
      read_schema_version("CMagnetCurrentScanPart", max_supported = 2)
  )
  # other fields
  data$start <- bfile |> read_binary_data("int")
  data$stop <- bfile |> read_binary_data("int")
  data$step <- bfile |> read_binary_data("int")
  data$delay <- bfile |> read_binary_data("int") # not 100% sure this is correct
  return(dplyr::as_tibble(data))
}

# read CScanPart::CIntegrationUnitScanPart (complete)
read_CIntegrationUnitScanPart <- function(bfile) {
  # parent and version
  data <- list(
    pCScanPart = bfile |> read_CScanPart() |> list(),
    version = bfile |>
      read_schema_version("CIntegrationUnitScanPart", max_supported = 3)
  )

  # fields
  data$xC0 <- bfile |> read_binary_data("int")
  data$xC4 <- bfile |> read_binary_data("uint8")

  return(dplyr::as_tibble(data))
}

# CData::CBasicInterface::CHardwarePart chain ====

# read CHardwarePart object (complete)
read_CHardwarePart <- function(bfile) {
  # parent + version
  data <- list(
    pCBasicInterface = bfile |> read_CBasicInterface() |> list(),
    version = bfile |> read_schema_version("CHardwarePart", max_supported = 10)
  )

  # CInterface-derived member
  data$CInterface <- bfile |> read_object(pattern = "Interface") |> list()

  # hasGasConfPart flag + expected object
  data$has_CGasConfPart <- bfile |> read_binary_data("bool")
  if (!is.na(data$has_CGasConfPart) && data$has_CGasConfPart) {
    data$CGasConfPart <- bfile |> read_object(pattern = "GasConfPart") |> list()
  }

  # hasMethodPart flag + expected object
  data$has_CMethodPart <- bfile |> read_binary_data("bool")
  if (!is.na(data$has_CMethodPart) && data$has_CMethodPart) {
    # FIXME: not implemented
    bfile |>
      register_cnd(cli_abort(
        "non-zero {.field CMethodPart} - not yet implemented"
      ))
  }

  # has_extra_CData flag + expected base (CData)
  data$has_extra_CData <- bfile |> read_binary_data("bool")
  if (!is.na(data$has_extra_CData) && data$has_extra_CData) {
    # are there possibilites other than CCalibration here?
    data$CData_extra <- bfile |> read_object(pattern = "CCalibration") |> list()
  }

  # version gated fields
  if (!is.na(data$version) && data$version >= 3) {
    data <- bfile |>
      read_binary_data_list(
        data = data,
        c(
          "xAC" = "bool",
          "xB0" = "bool",
          "xB4" = "bool",
          "xB8" = "bool"
        )
      )

    # version 7
    if (!is.na(data$version) && data$version >= 7) {
      # CVisualization data
      data$CVisualisationData <- bfile |>
        read_object("CVisualisationData") |>
        list()
      data$xC8 <- bfile |> read_binary_data("double") # value tends to be 10,000.00
      data$xBC <- bfile |> read_binary_data("int") # might be a visualization flag (bool)

      # version 9
      if (!is.na(data$version) && data$version >= 9) {
        data$n_strings1 <- bfile |> read_binary_data("int") # 0xFC
        if (!is.na(data$n_strings1) && data$n_strings1 > 0) {
          bfile |>
            register_cnd(cli_abort(
              "non-zero {.field CStringArray} - not yet implemented"
            ))
        }
        data$n_strings2 <- bfile |> read_binary_data("int") # 0x110
        if (!is.na(data$n_strings2) && data$n_strings2 > 0) {
          bfile |>
            register_cnd(cli_abort(
              "non-zero {.field CStringArray} - not yet implemented"
            ))
        }

        # version 10
        if (!is.na(data$version) && data$version >= 10) {
          data$xA4 <- bfile |> read_binary_data("string") # user-defined GUI text
        }
      }
    }
  }

  return(dplyr::as_tibble(data))
}

# read CHardwarePart::CCupHardwarePart (complete)
read_CCupHardwarePart <- function(bfile) {
  # parent and version
  data <- list(
    pCHardwarePart = bfile |> read_CHardwarePart() |> list(),
    version = bfile |>
      read_schema_version("CCupHardwarePart", max_supported = 5)
  )

  # fields
  data$mode <- bfile |> read_binary_data("uint8") # values observed: 0,1,2
  data$resistor <- bfile |> read_binary_data("double")
  data$x138 <- bfile |> read_binary_data("double") # values observed: 0 and 2.5

  # version gated fields
  if (!is.na(data$version) && data$version >= 3) {
    data$x130 <- bfile |> read_binary_data("double") # values observed: 0 or same as resistor

    if (!is.na(data$version) && data$version == 4) {
      # 24 bytes of legacy ata that only existed in version 4 --> read but not stored
      bfile |> read_binary_data("raw", n = 24L)
    }
  }
  return(dplyr::as_tibble(data))
}

# read CHardwarePart::CChannelHardwarePart (complete)
read_CChannelHardwarePart <- function(bfile) {
  # parent and version
  data <- list(
    pCHardwarePart = bfile |> read_CHardwarePart() |> list(),
    version = bfile |>
      read_schema_version("CChannelHardwarePart", max_supported = 2)
  )

  # fields
  data$x120 <- bfile |> read_binary_data("int")
  data$x124 <- bfile |> read_binary_data("int")
  return(dplyr::as_tibble(data))
}

# read CHardwarePart::CScaleHardwarePart (complete)
read_CScaleHardwarePart <- function(bfile) {
  # parent and version
  data <- list(
    pCHardwarePart = bfile |> read_CHardwarePart() |> list(),
    version = bfile |>
      read_schema_version("CScaleHardwarePart", max_supported = 12)
  )

  # fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "units" = "string",
        "min_step" = "uint32", # 0x180
        "max_step" = "uint32" # 0x184
      )
    )

  # version gated fields
  if (!is.na(data$version) && data$version >= 4) {
    data$format_mask <- bfile |> read_binary_data("uint32") # 0x120
    # version 5
    if (!is.na(data$version) && data$version >= 5) {
      data <- bfile |>
        read_binary_data_list(
          data = data,
          c(
            # these (and subsequent uint32s) are potentially mostly flags (booleans)
            "x124" = "uint32",
            "x128" = "uint32",
            "x130" = "uint32",
            "x134" = "uint32",
            "x140" = "uint32"
          )
        )

      # version 6
      if (!is.na(data$version) && data$version >= 6) {
        data$x144 <- bfile |> read_binary_data("uint32")
        data$x148 <- bfile |> read_binary_data("uint32")
        data$x14C <- bfile |> read_binary_data("uint32")

        # version 7
        if (!is.na(data$version) && data$version >= 7) {
          data$x150 <- bfile |> read_binary_data("uint32")

          # version 8
          if (!is.na(data$version) && data$version >= 8) {
            data$x154 <- bfile |> read_binary_data("string")
            data$x158 <- bfile |> read_binary_data("uint32")

            # version 9
            if (!is.na(data$version) && data$version >= 9) {
              data$x138 <- bfile |> read_binary_data("uint32")
              data$x13C <- bfile |> read_binary_data("uint32")

              # version 10
              if (!is.na(data$version) && data$version >= 10) {
                data$x15C <- bfile |> read_binary_data("string")

                # version 11
                if (!is.na(data$version) && data$version >= 11) {
                  data$x160 <- bfile |> read_binary_data("uint32")
                  data$x164 <- bfile |> read_binary_data("uint32")
                }

                # version 12
                if (!is.na(data$version) && data$version >= 12) {
                  data$min_value <- bfile |> read_binary_data("double") # 0x168, defaults to -10,000.00
                  data$max_value <- bfile |> read_binary_data("double") # 0x170 defaults to +10,000.00
                }
              }
            }
          }
        }
      }
    }
  }

  return(dplyr::as_tibble(data))
}

# read CHardwarePart::CScaleHardwarePart::CClockHardwarePart (complete)
read_CClockHardwarePart <- function(bfile) {
  data <- list(
    pCScaleHardwarePart = bfile |> read_CScaleHardwarePart() |> list(),
    version = bfile |>
      read_schema_version("CClockHardwarePart", max_supported = 2)
  )
  data$x190 <- bfile |> read_binary_data("uint32")
  return(dplyr::as_tibble(data))
}

# read CHardwarePart::CScaleHardwarePart::CIntegrationUnitHardwarePart (complete)
read_CIntegrationUnitHardwarePart <- function(bfile) {
  # parent and version
  data <- list(
    pCScaleHardwarePart = bfile |> read_CScaleHardwarePart() |> list(),
    version = bfile |>
      read_schema_version("CIntegrationUnitHardwarePart", max_supported = 3)
  )

  # fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "x194" = "int",
        "x198" = "uint8",
        "x19C" = "int", # constructor initializes to 1000
        "x1A0" = "int",
        "x199" = "uint8",
        "x190" = "uint8",
        "n_integration_times" = "uint8" # 0x1B8
      )
    )

  if (!is.na(data$n_integration_times) && data$n_integration_times > 0) {
    # most likely in miliseconds
    data$integration_times <- bfile |>
      read_binary_data("uint16", n = data$n_integration_times) |>
      list() # 0x1B4
  }

  # read cup definitions
  data$n_cups <- bfile |> read_binary_data("uint8") # 0x1E0
  if (!is.na(data$n_cups) && data$n_cups > 0) {
    data$CCupHardwarePart <-
      1:data$n_cups |>
      purrr::map(
        ~ bfile |> read_object("CCupHardwarePart")
      ) |>
      dplyr::bind_rows() |>
      list()
  }

  # read channel definitions
  data$n_channels <- bfile |> read_binary_data("uint8") # 0x1CC
  if (!is.na(data$n_channels) && data$n_channels > 0) {
    data$CChannelHardwarePart <-
      1:data$n_channels |>
      purrr::map(
        ~ bfile |> read_object("CChannelHardwarePart")
      ) |>
      dplyr::bind_rows() |>
      list()
  }

  # version gated fields
  if (!is.na(data$version) && data$version >= 3) {
    data$x1A8 <- bfile |> read_binary_data("bool") # gui checkbox
    data$x1AC <- bfile |> read_binary_data("bool") # gui checkbox
  }

  return(dplyr::as_tibble(data))
}

# read CHardwarePart::CScaleHardwarePart::CDacHardwarePart (complete)
read_CDacHardwarePart <- function(bfile) {
  data <- list(
    pCScaleHardwarePart = bfile |> read_CScaleHardwarePart() |> list(),
    version = bfile |>
      read_schema_version("CDacHardwarePart", max_supported = 3)
  )

  # fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "x190" = "uint8",
        "x191" = "uint8",
        "x192" = "uint8",
        "x193" = "uint8"
      )
    )

  # version gated fields
  if (!is.na(data$version) && data$version >= 3) {
    data$format <- bfile |> read_binary_data("string") # 0x194, looks like a sprintf format
  }

  return(dplyr::as_tibble(data))
}

# read CHardwarePart::CScaleHardwarePart::CDacHardwarePart::CScaleHvHardwarePart (complete)
read_CScaleHvHardwarePart <- function(bfile) {
  # parent and version
  data <- list(
    pCDacHardwarePart = bfile |> read_CDacHardwarePart() |> list(),
    version = bfile |>
      read_schema_version("CScaleHvHardwarePart", max_supported = 3)
  )

  # version gated fields
  if (!is.na(data$version) && data$version >= 3) {
    data$x198 <- bfile |> read_binary_data("double")
  }

  return(dplyr::as_tibble(data))
}

# read CHardwarePart::CScaleHardwarePart::CDacHardwarePart::CMagnetCurrentHardwarePart (complete)
read_CMagnetCurrentHardwarePart <- function(bfile) {
  # parent and version
  data <- list(
    pCDacHardwarePart = bfile |> read_CDacHardwarePart() |> list(),
    version = bfile |>
      read_schema_version("CMagnetCurrentHardwarePart", max_supported = 2)
  )

  # fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "x198" = "int", # maybe some sort of wait time in ms (10000)
        "x19C" = "int" # maybe some sort of wait time in ms (10000)
      )
    )

  return(dplyr::as_tibble(data))
}


# CSimple chain ======

# read CSimple object (complete)
read_CSimple <- function(bfile) {
  # fields
  data <-
    list(
      version = bfile |> read_schema_version("CSimple", max_supported = 2),
      "label" = bfile |> read_binary_data("string")
    )

  return(dplyr::as_tibble(data))
}

# read CSimple::CDword object (complete)
read_CDword <- function(bfile) {
  # version
  data <- list(
    pCSimple = bfile |> read_CSimple() |> list(),
    version = bfile |> read_schema_version("CDword", max_supported = 2)
  )
  # fields
  data$value <- bfile |> read_binary_data("int")
  return(dplyr::as_tibble(data))
}

# read CSimple::CDword::CPeakCenterOffset (same as the parent)
read_CPeakCenterOffset <- read_CDword


# read CSimple::CBinary object (complete)
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


# CPlotInfo & children =========

# read CPlotInfo object (complete)
# TODO: the meaning of the fields can all be figured out by using a scn file to
# change right-click-->Options one at a time and see what's what
read_CPlotInfo <- function(bfile) {
  # no version serialized beyond what MFC object serializes --> pull it from there
  version <- bfile$index |>
    dplyr::filter(.data$obj_idx == bfile$current_obj_idx) |>
    dplyr::pull(version)

  # fields
  data <- bfile |>
    read_binary_data_list(
      c(
        "x10" = "int", # probably a toggle, default 1
        "x20" = "int", # probably a toggle, default 0
        "x14" = "int", # probably a toggle, default 1
        "x18" = "int", # probably a toggle, default 1
        "x1c" = "int", # probably a toggle, default 1
        "right_left_factor" = "float", # pretty sure "Right left faktor" in Options, usually 10,000.00
        "background_color" = "color", # background color, default FF FF FF 00 (=white)
        "labels_color" = "color", # label color, default 00 00 00 00 (=black)
        "x38" = "int", # potentially linewidth (or another toggle), default 1
        "x3c" = "uint16", # potentially font size (default value is 0E 00 = 14)
        "font" = "string", # usually Arial
        "x_label" = "string", # without units!
        "y_label" = "string", # without units!
        "trace" = "string" # usually Ratio
      )
    )

  # CTrace Info
  data$CTraceInfo <- bfile |> read_object("CTraceInfo") |> list()

  # CPlotRange x2
  data$CPlotRange <- bfile |> read_object("CPlotRange") |> list()
  data$CPlotRange_zoom <- bfile |> read_object("CPlotRange") |> list()

  # additional indices (only if version > 1)
  if (!is.na(version) && version > 1) {
    data$x08 <- bfile |> read_binary_data("int") # potentially toggle, usually 1
    data$x0c <- bfile |> read_binary_data("int") # potentially toggle, usually 0
  }

  # then we have again a repeat of just the CPlotRange data (CPlotRange_zoom first this time)
  # FIXME: does this always happen for CPlotRange or is this a consequence of the second
  # point in CScanStorage (0x140) only? i.e. should this be here or elsewhere?
  data$CPlotRange_zoom2 <- bfile |> read_CPlotRange() |> list()
  data$CPlotRange2 <- bfile |> read_CPlotRange() |> list()

  # then we have again the trace labels
  # this information is redundant with that's in CTraceInfo
  n_traces <- data$CTraceInfo[[1]]$n_traces
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

# read CTraceInfo object (complete)
# TODO: the meaning of the fields can all be figured out by using a scn file to
# change right-click-->Options-->Traces one at a time and see what's what
read_CTraceInfo <- function(bfile) {
  # no version serialized!
  # fields
  data <- bfile |>
    read_binary_data_list(
      c(
        # todo: these can maybe be figured out by using a scn file to
        # change right-click-->Options one at a time and see what's what
        "x04" = "int", # potentially another color, default FF FF FF 00 (=white)
        "n_traces" = "uint8" # 0x44, number of traces
      )
    )

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

# read CTraceInfoEntry object (complete)
read_CTraceInfoEntry <- function(bfile) {
  # no version serialized!
  data <- bfile |>
    read_binary_data_list(
      c(
        "idx" = "uint8", # 0, 1, 2, etc.
        "x05" = "raw", # usually FF but could be some additionl index
        "trace_color" = "color", # 0x08 the trace color: 08 00 00 00 (= maroon), 00 08 00 00 (= medium green), 00 00 08 00 (= navy blue)
        "x0c" = "int", # probably a toggle, usually 0
        "x10" = "int", # probably a toggle, usually 0
        "x14" = "int" # probably a toggle, usually 1
      )
    )
  return(dplyr::as_tibble(data))
}

# read CPlotRange object (complete)
read_CPlotRange <- function(bfile) {
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
  return(dplyr::as_tibble(data))
}
