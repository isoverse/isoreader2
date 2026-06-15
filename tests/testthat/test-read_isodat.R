# Tests for the isodat data-table readers. These use small hand-written JSON
# fixtures that mimic the isoextract output structure, so they exercise the
# parsing logic without needing isoextract or any real data files.

write_json_fixture <- function(json, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  writeLines(json, f)
  f
}

test_that("read_isodat_eval_cells() reads numeric, integer, and string cells", {
  f <- write_json_fixture(
    r'({
      "objects": {
        "CEvalDataDoubleTransferPart": [
          {"idx": 3, "p": {"p": {"name": "rIntensity 28", "units": "[mVs]"}, "data": 57524.3}},
          {"idx": 5, "p": {"p": {"name": "Sample Dilution", "units": "[%]"}, "data": 0.0}}
        ],
        "CEvalDataIntTransferPart": {"idx": 2, "p": {"p": {"name": "Nr.", "units": " "}, "data": 1}},
        "CEvalDataStringTransferPart": {"idx": 20, "p": {"p": {"name": "Ref. Name", "units": " "}}, "data_string": "N2_zero"}
      }
    })'
  )
  cells <- read_isodat_eval_cells(f, "/objects")

  # ordered by idx; units appended only when present
  expect_equal(
    names(cells),
    c("Nr.", "rIntensity 28 [mVs]", "Sample Dilution [%]", "Ref. Name")
  )
  # integer cell -> integer; double cells -> double; string -> character
  expect_identical(cells[["Nr."]], 1L)
  expect_identical(cells[["rIntensity 28 [mVs]"]], 57524.3)
  expect_identical(cells[["Sample Dilution [%]"]], 0)
  expect_identical(cells[["Ref. Name"]], "N2_zero")

  # missing objects node -> NULL
  expect_null(read_isodat_eval_cells(f, "/does/not/exist"))
})

test_that("read_isodat_gc_peak_geometry() reads RT window and per-mass values", {
  f <- write_json_fixture(
    r'({
      "cspeak": {"p": {"p": {"objects": {"CGCPeak": [
        {"idx": 1, "p": {"mass": 28, "bgd0": 35.1}, "start_rt": 40.1, "apex_rt": 60.0, "end_rt": 63.3, "apex_signal": 3024.0},
        {"idx": 2, "p": {"mass": 29, "bgd0": 36.2}, "start_rt": 40.1, "apex_rt": 60.2, "end_rt": 63.3, "apex_signal": 2194.0}
      ]}}}}
    })'
  )
  geom <- read_isodat_gc_peak_geometry(f, "/cspeak")

  expect_equal(
    names(geom),
    c(
      "Start [s]",
      "Rt [s]",
      "End [s]",
      "Ampl 28 [mV]",
      "Ampl 29 [mV]",
      "BGD 28 [mV]",
      "BGD 29 [mV]"
    )
  )
  # Start/End shared; Rt is from the first (major) trace
  expect_equal(geom[["Start [s]"]], 40.1)
  expect_equal(geom[["Rt [s]"]], 60.0)
  expect_equal(geom[["End [s]"]], 63.3)
  # per-mass amplitude (apex_signal) and background (bgd0)
  expect_equal(geom[["Ampl 28 [mV]"]], 3024.0)
  expect_equal(geom[["Ampl 29 [mV]"]], 2194.0)
  expect_equal(geom[["BGD 29 [mV]"]], 36.2)

  # single-trace peak (CGCPeak as a single object, not a list)
  f2 <- write_json_fixture(
    r'({
      "cspeak": {"p": {"p": {"objects": {"CGCPeak":
        {"idx": 1, "p": {"mass": 2, "bgd0": 10.0}, "start_rt": 5.0, "apex_rt": 7.0, "end_rt": 9.0, "apex_signal": 100.0}
      }}}}
    })'
  )
  geom2 <- read_isodat_gc_peak_geometry(f2, "/cspeak")
  expect_equal(
    names(geom2),
    c("Start [s]", "Rt [s]", "End [s]", "Ampl 2 [mV]", "BGD 2 [mV]")
  )
  expect_equal(geom2[["Ampl 2 [mV]"]], 100.0)

  # absent CGCPeak -> NULL
  expect_null(read_isodat_gc_peak_geometry(f, "/missing"))
})

test_that("read_isodat_gc_peak_table() combines geometry, cells, and species", {
  one_peak <- function(nr, gas, rt, ampl, delta) {
    sprintf(
      '{"p": {"gas_name": "%s",
        "p": {"objects": {"CGCPeak": {"idx": 1, "p": {"mass": 28, "bgd0": 35.0}, "start_rt": %s, "apex_rt": %s, "end_rt": %s, "apex_signal": %s}}},
        "CEvalDataItemListTransferPart": {"p": {"objects": {
          "CEvalDataIntTransferPart": {"idx": 1, "p": {"p": {"name": "Nr.", "units": " "}, "data": %d}},
          "CEvalDataDoubleTransferPart": {"idx": 2, "p": {"p": {"name": "d 29N2/28N2", "units": "[per mil]"}, "data": %s}}
        }}}
      }}',
      gas,
      rt - 1,
      rt,
      rt + 1,
      ampl,
      nr,
      delta
    )
  }
  f <- write_json_fixture(sprintf(
    '{"peaklist": {"p": {"n_objects": 2, "objects": {"CSPeak": [%s, %s]}}}}',
    one_peak(1, "N2", 60, 3024, 0.5),
    one_peak(2, "N2", 111, 3000, 1.05)
  ))
  tab <- read_isodat_gc_peak_table(f, "/peaklist")

  expect_s3_class(tab, "tbl_df")
  expect_equal(nrow(tab), 2L)
  expect_equal(
    names(tab),
    c(
      "analysis",
      "species",
      "Start [s]",
      "Rt [s]",
      "End [s]",
      "Ampl 28 [mV]",
      "BGD 28 [mV]",
      "Nr.",
      "d 29N2/28N2 [per mil]"
    )
  )
  expect_equal(tab$species, c("N2", "N2"))
  expect_equal(tab$Nr., c(1L, 2L))
  expect_equal(tab[["Rt [s]"]], c(60, 111))
  expect_equal(tab[["d 29N2/28N2 [per mil]"]], c(0.5, 1.05))

  # empty / absent peak list -> NULL
  empty <- write_json_fixture('{"peaklist": {"p": {"n_objects": 0}}}')
  expect_null(read_isodat_gc_peak_table(empty, "/peaklist"))
})

test_that("ir_read_isofiles() populates vendor_data_table from a real file", {
  skip_on_cran()
  # reading requires the bundled isoextract binary
  skip_if(
    !file.exists(get_isoextract_path()),
    "isoextract binary is not installed"
  )

  # copy an example file to a temp folder so the .json sidecar is not written
  # into the installed package's extdata directory
  data_dir <- withr::local_tempdir()
  file.copy(
    file.path(ir_examples_folder(), "continuous_flow_ea_example.dxf"),
    data_dir
  )

  iso <- data_dir |>
    ir_find_continuous_flow() |>
    ir_read_isofiles(show_progress = FALSE, show_problems = FALSE) |>
    suppressMessages()

  expect_true("vendor_data_table" %in% names(iso))
  dt <- iso$vendor_data_table[[1]]
  expect_s3_class(dt, "tbl_df")
  expect_gt(nrow(dt), 0L)

  # evaluated results, retention-time geometry, and string columns are present
  expect_true(all(
    c("species", "Nr.", "Start [s]", "Rt [s]", "End [s]", "Ref. Name") %in%
      names(dt)
  ))
  # units carried into column names; integer typing for the peak number
  expect_true(any(grepl(" \\[mVs\\]$", names(dt))))
  expect_type(dt[["Nr."]], "integer")
  expect_type(dt[["Ref. Name"]], "character")
})

test_that("read_did_data_table() pivots evaluated columns to cycle x column", {
  col <- function(name, ys) {
    sprintf(
      '{"p": {"p": {"v": "%s"}, "objects": {"CTwoDoublesArrayData": {"x_data": [0, 1, 2], "y_data": [%s]}}}}',
      name,
      paste(ys, collapse = ", ")
    )
  }
  # the "Evaluated Results" CBlockData is located by its p/v label
  f <- write_json_fixture(sprintf(
    '{"CDualInletBlockData": {"p": {"objects": {"CBlockData": [
      {"p": {"v": "Pre Calculated"}, "objects": {}},
      {"p": {"v": "Evaluated Results"}, "objects": {"CDualInletEvaluatedDataCollect": {"p": {"objects": {"CDualInletEvaluatedData": [%s, %s]}}}}}
    ]}}}}',
    col("d 45CO2/44CO2 ", c(3.3, 3.2, 3.3)),
    col("AT% 13C/12C ", c(1.1, 1.1, 1.1))
  ))
  did <- read_did_data_table(f)

  expect_s3_class(did, "tbl_df")
  # trailing spaces in column names are trimmed; cycle is 1-based
  expect_equal(
    names(did),
    c("analysis", "cycle", "d 45CO2/44CO2", "AT% 13C/12C")
  )
  expect_equal(did$cycle, c(1L, 2L, 3L))
  expect_equal(did[["d 45CO2/44CO2"]], c(3.3, 3.2, 3.3))
  expect_equal(did[["AT% 13C/12C"]], c(1.1, 1.1, 1.1))
})

test_that("ir_read_isofiles() reads every isodat example type", {
  skip_on_cran()
  skip_if(
    !file.exists(get_isoextract_path()),
    "isoextract binary is not installed"
  )

  # copy all bundled examples to a temp folder (so .json sidecars are not
  # written into the installed package) and read them
  data_dir <- withr::local_tempdir()
  file.copy(
    list.files(ir_examples_folder(), full.names = TRUE),
    data_dir
  )

  read_one <- function(pattern) {
    data_dir |>
      ir_find_isofiles(pattern = pattern) |>
      ir_read_isofiles(show_progress = FALSE, show_problems = FALSE) |>
      suppressMessages()
  }

  # continuous flow: .dxf (CResultArray) and .cf (CBlockDataContext)
  cf <- read_one("continuous_flow")
  expect_equal(nrow(cf), 2L)
  for (i in seq_len(nrow(cf))) {
    expect_true(all(c("metadata", "traces") %in% names(cf)))
    expect_gt(nrow(cf$traces[[i]]), 0L)
    expect_equal(cf$metadata[[i]]$type, "cf")
  }

  # dual inlet: .caf (CBlockDataContext)
  di <- read_one("dual_inlet")
  expect_gt(nrow(di), 0L)
  for (i in seq_len(nrow(di))) {
    expect_gt(nrow(di$cycles[[i]]), 0L)
    expect_equal(di$metadata[[i]]$type, "di")
  }

  # scans: .scn (CScanStorage) — several scan types among the examples
  sc <- read_one("scan")
  expect_gt(nrow(sc), 0L)
  for (i in seq_len(nrow(sc))) {
    expect_gt(nrow(sc$scans[[i]]), 0L)
    expect_equal(sc$metadata[[i]]$type, "scan")
  }
})
