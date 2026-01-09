test_that("load_binary_file()", {
  # errors
  load_binary_file(1) |> expect_error("must be.*existing.*file")
  load_binary_file("DNE") |> expect_error("must be.*existing.*file")
})
