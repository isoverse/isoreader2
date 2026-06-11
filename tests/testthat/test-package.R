test_that("ir_examples_folder() points at the bundled examples", {
  folder <- ir_examples_folder()
  expect_type(folder, "character")
  expect_length(folder, 1L)
  expect_true(dir.exists(folder))
  # the bundled example files live here
  expect_true(
    length(list.files(folder, pattern = "\\.(dxf|cf|did|caf|scn)$")) > 0
  )
  # composes with the find functions
  expect_true(length(ir_find_scans(folder)) > 0)
})

test_that("ir_get_supported_file_types() returns the supported types", {
  types <- ir_get_supported_file_types()
  expect_s3_class(types, "tbl_df")
  expect_true(all(
    c("file_type", "min_isoextract_version", "vendor_software") %in%
      names(types)
  ))
  expect_true(all(
    c("dxf", "cf", "iarc", "larc", "bch", "imexp", "did", "caf", "scn") %in%
      types$file_type
  ))
})
