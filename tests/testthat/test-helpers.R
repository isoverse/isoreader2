test_that("factor_in_order()", {
  f <- factor_in_order(c("b", "a", "b", "c", "a"))
  expect_s3_class(f, "factor")
  # levels follow first-appearance order, not alphabetical
  expect_equal(levels(f), c("b", "a", "c"))
  # an existing factor is re-leveled by appearance too
  expect_equal(levels(factor_in_order(factor(c("z", "a")))), c("z", "a"))
})

test_that("check_arg()", {
  # passing condition returns invisibly without error
  expect_no_error(check_arg(5, is.numeric(5), "must be numeric"))

  # failing condition aborts with the message (and friendly type by default)
  expect_error(
    check_arg(5L, is.character(5L), "must be a string"),
    "must be a string"
  )
  expect_error(
    check_arg(5L, is.character(5L), "must be a string"),
    "not an integer"
  )

  # include_value adds the offending value to the message
  expect_error(
    check_arg(5L, is.character(5L), "must be a string", include_value = TRUE),
    "\\(5\\)"
  )

  # a missing argument is reported as such
  f <- function(x) check_arg(x, !missing(x), "must be provided")
  expect_error(f(), "not missing")
})

test_that("check_tibble()", {
  expect_no_error(check_tibble(tibble(a = 1, b = 2)))
  # not a data frame
  expect_error(check_tibble(42), "must be a data frame")
  # missing required columns
  expect_error(
    check_tibble(tibble(a = 1), req_cols = c("a", "b")),
    "column.*b.*missing"
  )
  expect_no_error(check_tibble(tibble(a = 1, b = 2), req_cols = c("a", "b")))
})
