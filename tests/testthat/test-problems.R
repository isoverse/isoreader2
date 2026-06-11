# a problems tibble as produced/stored during reading
make_problems <- function(messages = character(0)) {
  tibble(
    type = rep("error", length(messages)),
    call = rep(NA_character_, length(messages)),
    message = messages,
    condition = vector("list", length(messages))
  )
}

test_that("ir_get_problems() input checks", {
  ir_get_problems() |> expect_error("must be a list or data frame")
  ir_get_problems(42) |> expect_error("must be a list or data frame")
})

test_that("ir_get_problems() reads problems from an ir_isofiles object", {
  iso <- structure(
    tibble(
      file_path = c("a.dxf", "b.dxf"),
      problems = list(make_problems("bad thing"), make_problems())
    ),
    class = c("ir_isofiles", "tbl_df", "tbl", "data.frame")
  )
  probs <- ir_get_problems(iso)
  expect_s3_class(probs, "tbl_df")
  expect_equal(nrow(probs), 1L)
  # the per-file message is prefixed with the file reference
  expect_match(probs$message, "a.dxf")
  expect_match(probs$message, "bad thing")
})

test_that("ir_get_problems() reads problems from an ir_aggregated_data object", {
  agg <- structure(
    list(
      metadata = tibble(
        uidx = 1:2,
        file_path = c("a.dxf", "b.dxf"),
        file_name = c("a", "b")
      ),
      problems = tibble(
        uidx = 1L,
        type = "warning",
        call = NA_character_,
        message = "aggregation hiccup",
        condition = list(NULL)
      )
    ),
    class = "ir_aggregated_data"
  )
  probs <- ir_get_problems(agg)
  expect_equal(nrow(probs), 1L)
  expect_match(probs$message, "aggregation hiccup")
  expect_match(probs$message, "a.dxf")
})

test_that("ir_get_problems() handles a plain list with a problems element", {
  obj <- list(problems = make_problems("oops"))
  expect_equal(nrow(ir_get_problems(obj)), 1L)
  # nothing to report
  expect_equal(nrow(ir_get_problems(list())), 0L)
})

test_that("ir_show_problems() prints without error and returns invisibly", {
  iso <- structure(
    tibble(
      file_path = "a.dxf",
      problems = list(make_problems("bad thing"))
    ),
    class = c("ir_isofiles", "tbl_df", "tbl", "data.frame")
  )
  # prints (a message) but does not error; a direct problems tibble works too
  expect_no_error(suppressMessages(ir_show_problems(iso)))
  expect_no_error(suppressMessages(ir_show_problems(make_problems("x"))))
})
