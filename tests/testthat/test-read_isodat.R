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
        {"idx": 1, "p": {"mass": 28, "bgd0": 35.1, "bgd1": 20.1}, "start_idx": 401, "apex_idx": 600, "end_idx": 633, "start_rt": 40.1, "apex_rt": 60.0, "end_rt": 63.3, "apex_signal": 3024.0, "square_peak": 0, "time_shift": 0.0},
        {"idx": 2, "p": {"mass": 29, "bgd0": 36.2, "bgd1": 16.6}, "start_idx": 401, "apex_idx": 602, "end_idx": 633, "start_rt": 40.1, "apex_rt": 60.2, "end_rt": 63.3, "apex_signal": 2194.0, "square_peak": 0, "time_shift": 0.18}
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
  # per-mass amplitude (apex_signal) and background (the relevant bgd is bgd1)
  expect_equal(geom[["Ampl 28 [mV]"]], 3024.0)
  expect_equal(geom[["Ampl 29 [mV]"]], 2194.0)
  expect_equal(geom[["BGD 29 [mV]"]], 16.6)

  # single-trace peak (CGCPeak as a single object, not a list)
  f2 <- write_json_fixture(
    r'({
      "cspeak": {"p": {"p": {"objects": {"CGCPeak":
        {"idx": 1, "p": {"mass": 2, "bgd0": 10.0, "bgd1": 4.0}, "start_idx": 50, "apex_idx": 70, "end_idx": 90, "start_rt": 5.0, "apex_rt": 7.0, "end_rt": 9.0, "apex_signal": 100.0, "square_peak": 1, "time_shift": 0.5}
      }}}}
    })'
  )
  geom2 <- read_isodat_gc_peak_geometry(f2, "/cspeak")
  expect_equal(
    names(geom2),
    c("Start [s]", "Rt [s]", "End [s]", "Ampl 2 [mV]", "BGD 2 [mV]")
  )
  expect_equal(geom2[["Ampl 2 [mV]"]], 100.0)
  expect_equal(geom2[["BGD 2 [mV]"]], 4.0)

  # debug mode adds the Start/Rt/End indices, the square-peak flag (0/1 ->
  # FALSE/TRUE), and the per-mass shifts
  withr::local_options(list(isoreader2.debug = TRUE))
  geom_dbg <- read_isodat_gc_peak_geometry(f, "/cspeak")
  expect_equal(
    names(geom_dbg),
    c(
      "Start [s]",
      "Rt [s]",
      "End [s]",
      "Ampl 28 [mV]",
      "Ampl 29 [mV]",
      "BGD 28 [mV]",
      "BGD 29 [mV]",
      "Start [idx]",
      "Rt [idx]",
      "End [idx]",
      "square peak",
      "Shift 28 [s]",
      "Shift 29 [s]"
    )
  )
  # indices come from the first (major) trace
  expect_equal(geom_dbg[["Start [idx]"]], 401)
  expect_equal(geom_dbg[["Rt [idx]"]], 600)
  expect_equal(geom_dbg[["End [idx]"]], 633)
  expect_identical(geom_dbg[["square peak"]], FALSE)
  expect_equal(geom_dbg[["Shift 29 [s]"]], 0.18)
  # a square peak flag of 1 -> TRUE
  expect_identical(
    read_isodat_gc_peak_geometry(f2, "/cspeak")[["square peak"]],
    TRUE
  )

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

test_that("read_dxf_metadata() survives a missing Sequence Line Information block", {
  # a .dxf JSON with a timestamp but no "Sequence Line Information" CBlockData
  f <- write_json_fixture(
    r'({
      "CFileHeader": {"p": {"objects": {"CTimeObject": {"datetime": "2026-06-12T21:26:44.0000000+00:00"}}}},
      "CContiniousFlowBlockData": {"p": {"objects": {"CBlockData": [
        {"p": {"v": "RawDataBlock"}, "objects": {}}
      ]}}}
    })'
  )

  res <- read_dxf_metadata(f) |> try_catch_cnds()

  # the missing block is surfaced as a (non-fatal) warning, not a hard error
  expect_equal(nrow(res$conditions), 1L)
  expect_equal(res$conditions$type, "warning")
  expect_match(res$conditions$message, "Sequence Line Information")

  # the rest of the metadata is still returned
  md <- res$result
  expect_s3_class(md, "tbl_df")
  expect_equal(nrow(md), 1L)
  expect_equal(md$type, "cf")
  expect_false(is.na(md$timestamp))
})

test_that("read_dxf_data_table() handles single- and multi-gas results", {
  # one gas's CGCPeakList with a single peak carrying one evaluated cell
  peaklist <- function(gas, area) {
    sprintf(
      '{"p": {"n_objects": 1, "objects": {"CSPeak": [
        {"p": {"gas_name": "%s", "CEvalDataItemListTransferPart": {"p": {"objects": {
          "CEvalDataDoubleTransferPart": {"idx": 0, "p": {"p": {"name": "Area", "units": "[mVs]"}, "data": %s}}
        }}}}}
      ]}}}',
      gas,
      area
    )
  }
  results_block <- function(result_for_gas) {
    sprintf(
      '{"CContiniousFlowBlockData": {"p": {"objects": {"CBlockData": [
        {"p": {"v": "Results"}, "objects": {"CResultArray": {"p": {"objects": {
          "CResultForGas": %s
        }}}}}
      ]}}}}',
      result_for_gas
    )
  }

  # single gas: CResultForGas is inlined directly (no integer index) - this used
  # to be missed entirely
  single <- read_dxf_data_table(write_json_fixture(results_block(
    sprintf('{"CGCPeakList": %s}', peaklist("Ar", 123.4))
  )))
  expect_s3_class(single, "tbl_df")
  expect_equal(nrow(single), 1L)
  expect_equal(single$species, "Ar")
  expect_equal(single[["Area [mVs]"]], 123.4)

  # multiple gases: CResultForGas is integer-indexed
  multi <- read_dxf_data_table(write_json_fixture(results_block(sprintf(
    '[{"CGCPeakList": %s}, {"CGCPeakList": %s}]',
    peaklist("N2", 1.0),
    peaklist("CO2", 2.0)
  ))))
  expect_equal(nrow(multi), 2L)
  expect_equal(multi$species, c("N2", "CO2"))
  expect_equal(multi[["Area [mVs]"]], c(1.0, 2.0))
})

test_that("ir_read_isofiles() reads every isodat example type", {
  skip_on_cran()
  skip_if(
    !file.exists(get_isoextract_path()),
    "isoextract binary is not installed"
  )

  # copy the original example files (not their bundled .json sidecars) to a temp
  # folder so this test actually exercises the isoextract binary extraction path
  # rather than reading the pre-extracted json, and so sidecars are not written
  # into the installed package
  data_dir <- withr::local_tempdir()
  originals <- list.files(ir_examples_folder(), full.names = TRUE)
  originals <- originals[tools::file_ext(originals) != "json"]
  file.copy(originals, data_dir)

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
