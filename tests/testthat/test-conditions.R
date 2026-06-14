test_that("empty_cnds_tibble()", {
  e <- empty_cnds_tibble()
  expect_s3_class(e, "tbl_df")
  expect_equal(nrow(e), 0L)
  expect_equal(names(e), c("type", "call", "message", "condition"))
})

test_that("try_catch_cnds() captures the result and conditions", {
  # plain success
  ok <- try_catch_cnds(1 + 1)
  expect_equal(ok$result, 2)
  expect_equal(nrow(ok$conditions), 0L)

  # a warning is captured but execution continues
  w <- try_catch_cnds({
    warning("careful")
    42
  })
  expect_equal(w$result, 42)
  expect_equal(w$conditions$type, "warning")
  expect_match(w$conditions$message, "careful")

  # an error is captured and the supplied error_value returned
  e <- try_catch_cnds(stop("boom"), error_value = -1)
  expect_equal(e$result, -1)
  expect_equal(e$conditions$type, "error")
  expect_match(e$conditions$message, "boom")
})

test_that("summarize_cnds() / format_cnds() produce text", {
  cnds <- try_catch_cnds(
    {
      warning("w1")
      stop("e1")
    },
    error_value = NA
  )$conditions
  expect_type(summarize_cnds(cnds), "character")
  expect_type(format_cnds(cnds), "character")
})

test_that("show_cnds() runs without error", {
  cnds <- try_catch_cnds(warning("hmm"))$conditions
  expect_no_error(suppressMessages(show_cnds(cnds)))
  # nothing to show for an empty set
  expect_no_error(suppressMessages(show_cnds(empty_cnds_tibble())))
})

test_that("abort_cnds() aborts only when there are conditions", {
  cnds <- try_catch_cnds(stop("boom"), error_value = NA)$conditions
  expect_error(abort_cnds(cnds))
  expect_no_error(abort_cnds(empty_cnds_tibble()))
})

test_that("warn_cnds() warns only when there are conditions", {
  cnds <- try_catch_cnds(warning("careful"))$conditions
  expect_warning(warn_cnds(cnds))
  expect_no_warning(warn_cnds(empty_cnds_tibble()))
})
