test_that("ir_export_to_excel() input checks", {
  skip_if_not_installed("openxlsx")
  df <- tibble(a = 1:2, b = c("x", "y"))

  # missing / invalid file
  ir_export_to_excel(df) |> expect_error("file.*must be a path")
  ir_export_to_excel(df, file = 42) |> expect_error("file.*must be a path")

  # no data frames provided
  ir_export_to_excel(file = tempfile()) |>
    expect_error("no data provided")

  # non-data-frame arguments
  ir_export_to_excel(df, 42, file = tempfile()) |>
    expect_error("must be a data frame")
  ir_export_to_excel(42, df, "x", file = tempfile()) |>
    expect_error("must be a data frame")

  # duplicate sheet names
  ir_export_to_excel(a = df, a = df, file = tempfile()) |>
    expect_error("sheet names must be unique")
  # duplicate after truncation to 31 characters
  long1 <- paste0(strrep("x", 31), "A")
  long2 <- paste0(strrep("x", 31), "B")
  args <- stats::setNames(list(df, df), c(long1, long2))
  do.call(ir_export_to_excel, c(args, list(file = tempfile()))) |>
    expect_error("unique")
})

test_that("ir_export_to_excel() sheet naming", {
  skip_if_not_installed("openxlsx")
  df1 <- tibble(a = 1:2)
  df2 <- tibble(b = 3:4)
  df3 <- tibble(c = 5:6)

  # all named
  f1 <- tempfile(fileext = ".xlsx")
  ir_export_to_excel(metadata = df1, traces = df2, file = f1) |>
    suppressMessages()
  expect_equal(openxlsx::getSheetNames(f1), c("metadata", "traces"))

  # all unnamed -> Sheet{position}
  f2 <- tempfile(fileext = ".xlsx")
  ir_export_to_excel(df1, df2, df3, file = f2) |> suppressMessages()
  expect_equal(openxlsx::getSheetNames(f2), c("Sheet1", "Sheet2", "Sheet3"))

  # mixed: unnamed entries use their position, not a running count of unnamed
  f3 <- tempfile(fileext = ".xlsx")
  ir_export_to_excel(df1, traces = df2, df3, file = f3) |> suppressMessages()
  expect_equal(openxlsx::getSheetNames(f3), c("Sheet1", "traces", "Sheet3"))

  # long names truncated to 31 characters
  f4 <- tempfile(fileext = ".xlsx")
  ir_export_to_excel(
    !!strrep("y", 40) := df1,
    file = f4
  ) |>
    suppressMessages()
  expect_equal(openxlsx::getSheetNames(f4), strrep("y", 31))
})

test_that("ir_export_to_excel() round-trips data and file handling", {
  skip_if_not_installed("openxlsx")
  md <- tibble(file_name = c("a", "b"), n = c(1L, 2L), d = c(1.5, 2.5))
  tr <- tibble(t = c(0, 1, 2), v = c(10.1, 20.2, 30.3))

  # .xlsx extension appended when absent, nested output dir created
  base <- file.path(withr::local_tempdir(), "sub", "out")
  ret <- ir_export_to_excel(metadata = md, traces = tr, file = base) |>
    suppressMessages()
  expect_true(file.exists(paste0(base, ".xlsx")))

  # values round-trip
  back_md <- openxlsx::read.xlsx(paste0(base, ".xlsx"), sheet = "metadata")
  expect_equal(back_md$file_name, md$file_name)
  expect_equal(back_md$n, md$n)
  expect_equal(back_md$d, md$d)
  back_tr <- openxlsx::read.xlsx(paste0(base, ".xlsx"), sheet = "traces")
  expect_equal(back_tr$v, tr$v)

  # returns the list of data frames invisibly when several are given
  expect_equal(ret, list(metadata = md, traces = tr))

  # returns the single data frame when only one is given (pipe-friendly)
  f <- tempfile(fileext = ".xlsx")
  ret1 <- md |> ir_export_to_excel(file = f) |> suppressMessages()
  expect_equal(ret1, md)
  expect_equal(openxlsx::getSheetNames(f), "Sheet1")
})
