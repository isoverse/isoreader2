test_that("c.ir_isofiles()", {
  make_isofiles <- function(...) {
    structure(
      tibble(...),
      class = c("ir_isofiles", "tbl_df", "tbl", "data.frame")
    )
  }

  a <- make_isofiles(file_name = c("a", "b"), x = 1:2)
  b <- make_isofiles(file_name = "c", x = 3L)

  out <- c(a, b)

  # type is preserved
  expect_s3_class(out, "ir_isofiles")
  expect_true(is(out, "ir_isofiles"))
  expect_identical(
    class(out),
    c("ir_isofiles", "tbl_df", "tbl", "data.frame")
  )

  # rows are row-bound (not appended as list elements)
  expect_equal(nrow(out), 3L)
  expect_equal(out$file_name, c("a", "b", "c"))
  expect_equal(out$x, 1:3)

  # bind_rows aligns columns and fills missing ones with NA
  d <- make_isofiles(file_name = "d", y = 99)
  out2 <- c(a, d)
  expect_s3_class(out2, "ir_isofiles")
  expect_equal(out2$file_name, c("a", "b", "d"))
  expect_equal(out2$x, c(1L, 2L, NA))
  expect_equal(out2$y, c(NA, NA, 99))

  # combining a single object is a no-op on content but keeps the class
  expect_equal(c(a), a)

  # combining more than two works
  out3 <- c(a, b, d)
  expect_s3_class(out3, "ir_isofiles")
  expect_equal(nrow(out3), 4L)
})
