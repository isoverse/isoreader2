write_json <- function(json, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  writeLines(json, f)
  f
}

test_that("json_missing()", {
  expect_true(json_missing(NA_complex_))
  expect_false(json_missing(1))
  expect_false(json_missing(NA))
})

test_that("query_json() retrieves values and tries paths in order", {
  f <- write_json('{"a": {"b": 42}, "c": [1, 2, 3]}')
  expect_equal(query_json(f, "/a/b"), 42)
  expect_equal(query_json(f, "/c"), c(1, 2, 3))
  # first matching path of several wins
  expect_equal(query_json(f, c("/missing", "/a/b")), 42)
})

test_that("query_json() handles missing paths", {
  f <- write_json('{"a": 1}')
  # required (default) aborts with a diagnosis of the first missing node
  expect_error(query_json(f, "/x/y/z"), "missing node")
  # required = FALSE returns the NA_complex_ sentinel
  expect_true(json_missing(query_json(f, "/x/y/z", required = FALSE)))
})

test_that("query_json(list_as_tibble = TRUE) returns a tibble", {
  f <- write_json('{"rows": [{"k": 1}, {"k": 2}]}')
  out <- query_json(f, "/rows", list_as_tibble = TRUE)
  expect_s3_class(out, "tbl_df")
})

test_that("find_json_block_idx_by_value()", {
  f <- write_json(
    '{"CBlockData": [{"p": {"v": "A"}}, {"p": {"v": "B"}}, {"p": {"v": "C"}}]}'
  )
  expect_equal(find_json_block_idx_by_value(f, "/CBlockData", "A"), 0L)
  expect_equal(find_json_block_idx_by_value(f, "/CBlockData", "C"), 2L)

  # not found: aborts when required, NA when not
  expect_error(
    find_json_block_idx_by_value(f, "/CBlockData", "Z"),
    "was not found"
  )
  expect_true(is.na(
    find_json_block_idx_by_value(f, "/CBlockData", "Z", required = FALSE)
  ))
})
