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

test_that("get_assembly_runtime() / get_assembly_path() resolve the platform", {
  rid <- get_assembly_runtime()
  # os-arch identifier matching the released executable names
  expect_match(rid, "^(osx|linux|win)-(x64|arm64)$")
  # executable paths are named "<tool>-<runtime>" (with .exe on Windows)
  for (tool in c("isoextract", "isosolfs")) {
    exe <- basename(get_assembly_path(tool))
    expect_true(startsWith(exe, paste0(tool, "-", rid)))
    expect_equal(grepl("\\.exe$", exe), startsWith(rid, "win"))
  }
  # the dedicated wrappers agree with the generic helper
  expect_identical(get_isoextract_path(), get_assembly_path("isoextract"))
  expect_identical(get_isosolfs_path(), get_assembly_path("isosolfs"))
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
