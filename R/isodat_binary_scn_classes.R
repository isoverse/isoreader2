# scan class reader =========

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

  data$version <- bfile |>
    read_schema_version("CScanStorage", max_supported = 6)

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
  # first set of fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        "comment" = "string", # +0xf4
        "n_points" = "int", # +0x168,
        "n_traces" = "int" # +0x16c
      )
    )

  # Cbinary object instance is next
  data$CBinary <- bfile |>
    read_object(
      "CBinary",
      n_points = data$n_points,
      n_traces = data$n_traces
    ) |>
    list()

  # a second instance but this one is empty --> not storing it
  bfile |> read_object("CBinary", read_data = FALSE) |> list()

  # CPlotInfo is serialized in a separate indepnendent index (i.e. index resets just for this)
  data$CPlotInfo <- bfile |>
    read_object("CPlotInfo", independent_index = "plot_index") |>
    list()

  # additional fields
  data <- bfile |>
    read_binary_data_list(
      data = data,
      c(
        timestamp_start = "timestamp", # 0xF8
        timestamp_end = "timestamp", # 0xFC
        x100 = "int", # some sort of flag?
        x104 = "uint8", # bool or enum?
        x108 = "string" # OS username?
      )
    )

  # first CScanPart
  data$CScanPart1 <- bfile |> read_object(pattern = "ScanPart") |> list()

  # second CScanPart
  data$CScanPart2 <- bfile |> read_object(pattern = "ScanPart") |> list()

  # additional fields
  data$xD8 <- bfile |> read_binary_data("int")
  data$xDC <- bfile |> read_binary_data("int")
  data$xE0 <- bfile |> read_binary_data("int")

  # CGasConfiguration
  data$CGasConfiguration <- bfile |> read_object("CGasConfiguration")

  # version gated fields
  if (!is.na(data$version) && data$version >= 3) {
    data$n_peak_list <- bfile |> read_binary_data("int")

    if (!is.na(data$n_peak_list) && data$n_peak_list > 0) {
      bfile |>
        register_cnd(cli_abort(
          "unexpectedly encountered {data$n_peak_list} CPeakList objects - not yet implemented"
        ))
    }
    # version 5
    if (!is.na(data$version) && data$version >= 5) {
      data$n_graphic_info <- bfile |> read_binary_data("int")
      if (!is.na(data$n_graphic_info) && data$n_graphic_info > 0) {
        bfile |>
          register_cnd(cli_abort(
            "unexpectedly encountered {data$n_graphic_info} CSimpleGraphicInfo objects - not yet implemented"
          ))
      }

      # version 5
      if (!is.na(data$version) && data$version >= 6) {
        data$n_custom <- bfile |> read_binary_data("int")
        if (!is.na(data$n_custom) && data$n_custom > 0) {
          bfile |>
            register_cnd(cli_abort(
              "unexpectedly encountered {data$data$n_custom} custom objects - not yet implemented"
            ))
        }
      }

      # there's a legacy field here in older versions, not worth deserializing
    }
  }

  return(dplyr::as_tibble(data))
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
