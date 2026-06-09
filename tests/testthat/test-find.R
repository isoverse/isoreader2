extdata <- system.file("extdata", package = "isoreader2")

# ir_find_isofiles() ==========================================================

test_that("ir_find_isofiles()", {
  # argument errors ============================================================
  expect_error(ir_find_isofiles(), "folder")
  expect_error(ir_find_isofiles("/no/such/dir"), "folder")
  expect_error(ir_find_isofiles(42L), "folder")
  expect_error(ir_find_isofiles(extdata, types = 1L), "types")
  expect_error(ir_find_isofiles(extdata, types = character(0)), "types")
  expect_error(ir_find_isofiles(extdata, types = "dxf", pattern = c("a", "b")), "pattern")
  expect_error(ir_find_isofiles(extdata, types = "dxf", pattern = TRUE), "pattern")
  expect_error(ir_find_isofiles(extdata, types = "dxf", recursive = "yes"), "recursive")
  expect_error(
    ir_find_isofiles(extdata, types = "dxf", recursive = c(TRUE, FALSE)),
    "recursive"
  )

  # return value ===============================================================
  files <- ir_find_isofiles(extdata, types = c("dxf", "cf"))
  expect_type(files, "character")
  expect_equal(files, sort(files))
  expect_equal(files, unique(files))
  expect_length(ir_find_isofiles(extdata, types = "xyz"), 0)

  # default types ==============================================================
  all_files <- ir_find_isofiles(extdata)
  expect_true(any(grepl("\\.dxf$", all_files, ignore.case = TRUE)))
  expect_true(any(grepl("\\.cf$", all_files, ignore.case = TRUE)))
  expect_true(any(grepl("\\.caf$", all_files, ignore.case = TRUE)))
  expect_true(any(grepl("\\.did$", all_files, ignore.case = TRUE)))
  expect_true(any(grepl("\\.scn$", all_files, ignore.case = TRUE)))

  # types parameter ============================================================
  dxf_files <- ir_find_isofiles(extdata, types = "dxf")
  expect_true(all(grepl("\\.dxf$", dxf_files, ignore.case = TRUE)))
  expect_false(any(grepl("\\.cf$", dxf_files, ignore.case = TRUE)))

  cf_files <- ir_find_isofiles(extdata, types = "cf")
  expect_true(all(grepl("\\.cf$", cf_files, ignore.case = TRUE)))
  expect_false(any(grepl("\\.dxf$", cf_files, ignore.case = TRUE)))

  expect_equal(
    ir_find_isofiles(extdata, types = c("dxf", "cf")),
    ir_find_isofiles(extdata, types = c("cf", "dxf"))
  )

  # pattern parameter ==========================================================
  filtered <- ir_find_isofiles(extdata, types = c("dxf", "cf"), pattern = "continuous_flow")
  expect_true(length(filtered) <= length(files))
  expect_true(all(grepl("continuous_flow", filtered)))
  expect_length(ir_find_isofiles(extdata, types = "dxf", pattern = "xyzzy_no_match"), 0)
  expect_equal(ir_find_isofiles(extdata, types = c("dxf", "cf"), pattern = NULL), files)

  # json sidecar deduplication =================================================
  tmp <- withr::local_tempdir()
  file.copy(file.path(extdata, "continuous_flow_ea_example.dxf"), tmp)
  file.copy(file.path(extdata, "continuous_flow_gc_example.cf"), tmp)
  file.create(file.path(tmp, "continuous_flow_ea_example.dxf.json"))

  tmp_files <- ir_find_isofiles(tmp, types = c("dxf", "cf"))
  expect_length(tmp_files[grepl("\\.dxf$", tmp_files, ignore.case = TRUE)], 1)
  expect_false(any(grepl("\\.json$", tmp_files, ignore.case = TRUE)))

  # recursive parameter ========================================================
  tmp2 <- withr::local_tempdir()
  subdir <- file.path(tmp2, "sub")
  dir.create(subdir)
  file.copy(file.path(extdata, "continuous_flow_ea_example.dxf"), tmp2)
  file.copy(file.path(extdata, "continuous_flow_gc_example.cf"), subdir)

  expect_length(ir_find_isofiles(tmp2, types = c("dxf", "cf"), recursive = TRUE), 2)
  expect_length(ir_find_isofiles(tmp2, types = c("dxf", "cf"), recursive = FALSE), 1)

  # multiple folders ===========================================================
  tmp3 <- withr::local_tempdir()
  tmp4 <- withr::local_tempdir()
  file.copy(file.path(extdata, "continuous_flow_ea_example.dxf"), tmp3)
  file.copy(file.path(extdata, "continuous_flow_gc_example.cf"), tmp4)

  multi_files <- ir_find_isofiles(c(tmp3, tmp4), types = c("dxf", "cf"))
  expect_length(multi_files, 2)
  expect_true(any(grepl("\\.dxf$", multi_files, ignore.case = TRUE)))
  expect_true(any(grepl("\\.cf$", multi_files, ignore.case = TRUE)))
  expect_equal(multi_files, sort(multi_files))
  expect_equal(multi_files, unique(multi_files))

  expect_error(ir_find_isofiles(c(tmp3, "/no/such/dir"), types = "dxf"), "folder")
})

# ir_find_continuous_flow() ===================================================

test_that("ir_find_continuous_flow()", {
  expect_error(ir_find_continuous_flow(), "folder")
  expect_error(ir_find_continuous_flow("/no/such/dir"), "folder")

  files <- ir_find_continuous_flow(extdata)
  expect_true(any(grepl("\\.dxf$", files, ignore.case = TRUE)))
  expect_true(any(grepl("\\.cf$", files, ignore.case = TRUE)))
  expect_false(any(grepl("\\.(did|caf|scn)$", files, ignore.case = TRUE)))
})

# ir_find_dual_inlet() ========================================================

test_that("ir_find_dual_inlet()", {
  expect_error(ir_find_dual_inlet(), "folder")
  expect_error(ir_find_dual_inlet("/no/such/dir"), "folder")

  files <- ir_find_dual_inlet(extdata)
  expect_true(any(grepl("\\.did$", files, ignore.case = TRUE)))
  expect_true(any(grepl("\\.caf$", files, ignore.case = TRUE)))
  expect_false(any(grepl("\\.(dxf|cf|scn)$", files, ignore.case = TRUE)))
})

# ir_find_scans() =============================================================

test_that("ir_find_scans()", {
  expect_error(ir_find_scans(), "folder")
  expect_error(ir_find_scans("/no/such/dir"), "folder")

  files <- ir_find_scans(extdata)
  expect_true(length(files) > 0)
  expect_true(all(grepl("\\.scn$", files, ignore.case = TRUE)))
  expect_false(any(grepl("\\.(dxf|cf|did|caf)$", files, ignore.case = TRUE)))
})
