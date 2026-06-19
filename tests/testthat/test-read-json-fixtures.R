# The core functionality of the package - reading the .json sidecars produced by
# isoextract - does not require the isoextract binary. The fixtures in
# test_files/ are pre-extracted .json files whose source binaries are
# intentionally absent, so ir_read_isofiles() reads them as-is (no re-extraction,
# no binary, no internet) and these tests run anywhere, including on CRAN.

test_that("ir_read_isofiles() reads pre-extracted .json fixtures without the binary", {
  files <- ir_find_isofiles(test_path("test_files"))
  expect_gt(length(files), 0L)

  iso <- ir_read_isofiles(
    files,
    show_progress = FALSE,
    show_problems = FALSE
  ) |>
    suppressMessages()

  expect_s3_class(iso, "ir_isofiles")
  expect_equal(nrow(iso), length(files))

  # nothing went wrong reading any of them
  expect_equal(sum(purrr::map_int(iso$problems, nrow)), 0L)

  # every read path is exercised: dxf + cf (continuous flow), caf (dual inlet),
  # scn (scans)
  types <- purrr::map_chr(iso$metadata, ~ .x$type)
  expect_setequal(unique(types), c("cf", "di", "scan"))

  # each file's measurement type comes with a populated matching dataset
  for (i in seq_len(nrow(iso))) {
    dataset <- switch(types[i], cf = "traces", di = "cycles", scan = "scans")
    expect_gt(nrow(iso[[dataset]][[i]]), 0L)
    # metadata always carries a timestamp parsed from the file
    expect_s3_class(iso$metadata[[i]]$timestamp, "POSIXct")
  }
})

test_that("each read_*_json() reader parses its fixture", {
  read_fixture <- function(name) {
    json <- test_path("test_files", name)
    ext <- tolower(tools::file_ext(sub("\\.json$", "", name)))
    reader <- get(
      sprintf("read_%s_json", ext),
      envir = asNamespace("isoreader2")
    )
    reader(json)
  }

  # .dxf continuous flow: traces, calibrated resistors, vendor data table
  dxf <- read_fixture("continuous_flow_ea_example.dxf.json")
  expect_equal(dxf$metadata[[1]]$type, "cf")
  expect_gt(nrow(dxf$traces[[1]]), 0L)
  expect_gt(nrow(dxf$resistors[[1]]), 0L)
  expect_gt(nrow(dxf$vendor_data_table[[1]]), 0L)

  # .cf continuous flow
  cf <- read_fixture("continuous_flow_gc_example.cf.json")
  expect_equal(cf$metadata[[1]]$type, "cf")
  expect_gt(nrow(cf$traces[[1]]), 0L)

  # .caf dual inlet: cycle data
  caf <- read_fixture("caf_dual_inlet_example.caf.json")
  expect_equal(caf$metadata[[1]]$type, "di")
  expect_gt(nrow(caf$cycles[[1]]), 0L)

  # .scn scan
  scn <- read_fixture("full_scan_example.scn.json")
  expect_equal(scn$metadata[[1]]$type, "scan")
  expect_gt(nrow(scn$scans[[1]]), 0L)
})
