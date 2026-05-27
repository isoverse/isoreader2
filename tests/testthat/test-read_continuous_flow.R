extdata <- system.file("extdata", package = "isoreader2")

test_that("ir_find_continuous_flow()", {
  # argument errors ============================================================
  expect_error(ir_find_continuous_flow(), "folder")
  expect_error(ir_find_continuous_flow("/no/such/dir"), "folder")
  expect_error(ir_find_continuous_flow(42L), "folder")
  expect_error(ir_find_continuous_flow(extdata, types = 1L), "types")
  expect_error(ir_find_continuous_flow(extdata, types = character(0)), "types")
  expect_error(
    ir_find_continuous_flow(extdata, pattern = c("a", "b")),
    "pattern"
  )
  expect_error(ir_find_continuous_flow(extdata, pattern = TRUE), "pattern")
  expect_error(ir_find_continuous_flow(extdata, recursive = "yes"), "recursive")
  expect_error(
    ir_find_continuous_flow(extdata, recursive = c(TRUE, FALSE)),
    "recursive"
  )

  # default parameters =========================================================
  files <- ir_find_continuous_flow(extdata)
  expect_type(files, "character")
  expect_true(any(grepl("\\.dxf$", files, ignore.case = TRUE)))
  expect_true(any(grepl("\\.cf$", files, ignore.case = TRUE)))
  expect_false(any(grepl(
    "\\.(scn|did|caf|txt|json)$",
    files,
    ignore.case = TRUE
  )))
  expect_equal(files, sort(files))
  expect_equal(files, unique(files))

  # types parameter ============================================================
  dxf_files <- ir_find_continuous_flow(extdata, types = "dxf")
  expect_true(all(grepl("\\.dxf$", dxf_files, ignore.case = TRUE)))
  expect_false(any(grepl("\\.cf$", dxf_files, ignore.case = TRUE)))

  cf_files <- ir_find_continuous_flow(extdata, types = "cf")
  expect_true(all(grepl("\\.cf$", cf_files, ignore.case = TRUE)))
  expect_false(any(grepl("\\.dxf$", cf_files, ignore.case = TRUE)))

  expect_equal(ir_find_continuous_flow(extdata, types = c("dxf", "cf")), files)
  expect_length(ir_find_continuous_flow(extdata, types = "xyz"), 0)

  # pattern parameter ==========================================================
  filtered <- ir_find_continuous_flow(extdata, pattern = "continuous_flow")
  expect_true(length(filtered) <= length(files))
  expect_true(all(grepl("continuous_flow", filtered)))
  expect_length(ir_find_continuous_flow(extdata, pattern = "xyzzy_no_match"), 0)
  expect_equal(ir_find_continuous_flow(extdata, pattern = NULL), files)

  # json sidecar deduplication =================================================
  tmp <- withr::local_tempdir()
  file.copy(file.path(extdata, "continuous_flow_example.dxf"), tmp)
  file.copy(file.path(extdata, "continuous_flow_example.cf"), tmp)
  file.create(file.path(tmp, "continuous_flow_example.dxf.json"))

  tmp_files <- ir_find_continuous_flow(tmp)
  expect_length(tmp_files[grepl("\\.dxf$", tmp_files, ignore.case = TRUE)], 1)
  expect_false(any(grepl("\\.json$", tmp_files, ignore.case = TRUE)))

  # recursive parameter ========================================================
  tmp2 <- withr::local_tempdir()
  subdir <- file.path(tmp2, "sub")
  dir.create(subdir)
  file.copy(file.path(extdata, "continuous_flow_example.dxf"), tmp2)
  file.copy(file.path(extdata, "continuous_flow_example.cf"), subdir)

  expect_length(ir_find_continuous_flow(tmp2, recursive = TRUE), 2)
  expect_length(ir_find_continuous_flow(tmp2, recursive = FALSE), 1)
})
