make_isofiles <- function(n = 2) {
  structure(
    tibble(
      file_path = paste0("f", seq_len(n), ".dxf"),
      metadata = lapply(seq_len(n), \(i) tibble(file_name = paste0("f", i))),
      problems = lapply(seq_len(n), \(i) tibble(message = character(0)))
    ),
    class = c("ir_isofiles", "tbl_df", "tbl", "data.frame")
  )
}

test_that("ir_save_isofiles() / ir_load_isofiles() input checks", {
  ir_save_isofiles() |>
    expect_error("must be a collection of isofiles")
  ir_save_isofiles(42, tempfile()) |>
    expect_error("must be a collection of isofiles")
  ir_save_isofiles(make_isofiles()) |> expect_error("file.*must be a path")

  ir_load_isofiles() |> expect_error("must be a string")
  ir_load_isofiles(42) |> expect_error("must be a string")

  # loading a file that is not an ir_isofiles object
  f <- tempfile(fileext = ".rds")
  saveRDS(42, f)
  ir_load_isofiles(f) |>
    expect_error("does not contain a collection of isofiles")
})

test_that("ir_save_isofiles() / ir_load_isofiles() round-trip", {
  iso <- make_isofiles(2)
  f <- tempfile()

  # save returns the input invisibly and appends .rds
  ret <- ir_save_isofiles(iso, f) |> suppressMessages()
  expect_identical(ret, iso)
  expect_true(file.exists(paste0(f, ".rds")))

  # load (with and without the extension) returns the object unchanged
  back <- ir_load_isofiles(f) |> suppressMessages()
  expect_identical(back, iso)
  expect_s3_class(back, "ir_isofiles")
  expect_identical(
    ir_load_isofiles(paste0(f, ".rds")) |> suppressMessages(),
    iso
  )

  # a pre-existing .rds path is used as-is (not doubled up)
  f2 <- tempfile(fileext = ".rds")
  ir_save_isofiles(iso, f2) |> suppressMessages()
  expect_true(file.exists(f2))
  expect_false(file.exists(paste0(f2, ".rds")))
})

test_that("ir_save_isofiles() messages pluralize correctly", {
  one <- make_isofiles(1)
  two <- make_isofiles(2)
  f <- tempfile()

  expect_message(ir_save_isofiles(one, f), "saved 1 isofile to")
  expect_message(ir_save_isofiles(two, f), "saved 2 isofiles to")
  expect_message(ir_load_isofiles(f), "loaded 2 isofiles from")
})
