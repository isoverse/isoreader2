test_that("ir_options() / ir_get_options() / ir_get_option()", {
  # all options as a named list
  opts <- ir_get_options()
  expect_type(opts, "list")
  expect_true(all(c("aggregators", "debug", "auto_use_ansi") %in% names(opts)))

  # ir_options() with no args also returns the full named list
  expect_named(ir_options(), names(opts))

  # single option retrieval
  expect_false(ir_get_option("debug"))

  # pattern subset
  expect_named(ir_get_options("debug"), "debug")
  expect_named(ir_get_options("ansi"), "auto_use_ansi")
  expect_length(ir_get_options("no_such_pattern"), 0L)

  # retrieving an undefined option errors
  expect_error(ir_get_option("does_not_exist"))

  # setting and restoring an option
  old <- ir_get_option("debug")
  withr::defer(ir_options(debug = old))
  ir_options(debug = TRUE)
  expect_true(ir_get_option("debug"))

  # an invalid value is rejected by the option's check function
  expect_error(ir_options(debug = "not a logical"))
})
