test_that("ir_aggregate_isofiles() input checks", {
  # character vector -> hint to run ir_read_isofiles() first
  ir_aggregate_isofiles(c("a.dxf", "b.dxf")) |>
    expect_error("not a character vector.*ir_read_isofiles")
  ir_aggregate_isofiles("a.dxf") |>
    expect_error("not a character vector.*ir_read_isofiles")

  # other invalid inputs -> generic isofiles error
  ir_aggregate_isofiles() |>
    expect_error("must be a collection of isofiles")
  ir_aggregate_isofiles(42) |>
    expect_error("must be a collection of isofiles")
})

test_that("drop_empty_datasets()", {
  agg <- list(
    metadata = tibble(uidx = 1:2, file_name = c("a", "b")),
    cycles = tibble(),
    scans = tibble(uidx = 1:2)
  )
  out <- drop_empty_datasets(agg)
  # zero-column cycles dropped, others kept
  expect_equal(names(out), c("metadata", "scans"))
  # non-data-frame entries are left untouched
  expect_equal(
    drop_empty_datasets(list(a = tibble(), b = "keep")),
    list(b = "keep")
  )
})

test_that("ir_start_aggregator()", {
  # errors
  expect_error(ir_start_aggregator(), "must be a string")

  # value
  expect_true(is(ir_start_aggregator("test"), "ir_aggregator"))

  # messages
  test_that_cli("cli", configs = c("plain", "fancy"), {
    expect_snapshot(ir_start_aggregator("test"))
  })
})

test_that("ir_add_to_aggregator()", {
  # errors
  ir_add_to_aggregator(42) |>
    expect_error("must be.*ir_aggregator.*tibble")
  ir_start_aggregator("test") |>
    ir_add_to_aggregator() |>
    expect_error("column.*must be a string")
  ir_start_aggregator("test") |>
    ir_add_to_aggregator("metadata") |>
    expect_error("column.*must be a string")
  ir_start_aggregator("test") |>
    ir_add_to_aggregator("metadata", "col", 42) |>
    expect_error("source.*must be.*character.*or list")
  ir_start_aggregator("test") |>
    ir_add_to_aggregator("metadata", "col", regexp = 42) |>
    expect_error("regexp.*must be TRUE or FALSE")
  ir_start_aggregator("test") |>
    ir_add_to_aggregator("metadata", "col", cast = 42) |>
    expect_error("cast.*must be a string")
  ir_start_aggregator("test") |>
    ir_add_to_aggregator("metadata", "col", func = 42) |>
    expect_error("func.*must be a string")
  ir_start_aggregator("test") |>
    ir_add_to_aggregator("metadata", "col", args = 42) |>
    expect_error("args.*must be a list")
  ir_start_aggregator("test") |>
    ir_add_to_aggregator("metadata", "col", cast = "DNE") |>
    expect_error("function.*could not be found")
  ir_start_aggregator("test") |>
    ir_add_to_aggregator("metadata", "col", func = "DNE") |>
    expect_error("function.*could not be found")
  ir_start_aggregator("test") |>
    ir_add_to_aggregator("metadata", "col", cast = "sqrt", default = "x") |>
    expect_error()

  # values
  expect_equal(
    ir_start_aggregator("test") |>
      ir_add_to_aggregator("metadata", "col") |> # overwritten in next
      ir_add_to_aggregator("metadata", "col", cast = "as.integer"),
    ir_start_aggregator("test") |>
      ir_add_to_aggregator("metadata", "col", cast = "as.integer")
  )

  # messages
  test_that_cli("cli", configs = c("plain", "fancy"), {
    ir_start_aggregator("test") |>
      ir_add_to_aggregator("metadata", "col") |>
      ir_add_to_aggregator("metadata", "num", cast = "as.integer") |>
      ir_add_to_aggregator(
        "metadata",
        "new",
        source = c("def", "alt def"),
        default = 4
      ) |>
      ir_add_to_aggregator(
        "metadata",
        "w\\1_\\2",
        "(\\d+)-(.*)",
        regexp = TRUE
      ) |>
      ir_add_to_aggregator(
        "metadata",
        "from_fun",
        cast = "as.integer",
        source = list(c("a", "b"), "x"),
        func = "mean"
      ) |>
      expect_snapshot()
  })
})

test_that("ir_register_aggregator() and ir_get_aggregator()", {
  # errors
  ir_register_aggregator(42) |>
    expect_error("must be.*ir_aggregator.*tibble")
  ir_start_aggregator("test") |>
    ir_register_aggregator(42) |>
    expect_error("name.*a string")
  ir_get_aggregator("dne") |>
    expect_error("not.*registered")

  # values
  agg <- ir_start_aggregator("test") |> ir_add_to_aggregator("metadata", "col")
  agg |> ir_register_aggregator()
  expect_equal(ir_get_option("aggregators")$test, agg)
  expect_equal(ir_get_aggregator("test"), agg)
})

test_that("c.ir_aggregated_data() row-binds datasets and re-indexes uidx", {
  mk <- function(uidx, fn) {
    structure(
      list(
        metadata = tibble(uidx = uidx, file_name = fn, x = uidx * 1.0),
        traces = tibble(uidx = uidx, v = uidx * 10L),
        problems = tibble(uidx = integer(0), message = character(0))
      ),
      class = "ir_aggregated_data"
    )
  }
  a <- mk(1:2, c("a", "b"))
  b <- mk(1:2, c("c", "d"))

  comb <- c(a, b)
  expect_s3_class(comb, "ir_aggregated_data")
  expect_equal(names(comb), c("metadata", "traces", "problems"))
  # files row-bound and uidx renumbered so they stay unique
  expect_equal(comb$metadata$file_name, c("a", "b", "c", "d"))
  expect_equal(comb$metadata$uidx, 1:4)
  # the linkage is preserved (traces uidx tracks the same re-indexing)
  expect_equal(comb$traces$uidx, 1:4)
  expect_equal(comb$traces$v, c(10L, 20L, 10L, 20L))

  # datasets present in only one object are kept; columns are unioned with NA
  d <- structure(
    list(
      metadata = tibble(uidx = 1L, file_name = "e", y = 9),
      cycles = tibble(uidx = 1L, d13C = -5)
    ),
    class = "ir_aggregated_data"
  )
  comb2 <- c(a, d)
  expect_equal(names(comb2), c("metadata", "traces", "problems", "cycles"))
  expect_equal(comb2$metadata$uidx, 1:3)
  expect_setequal(names(comb2$metadata), c("uidx", "file_name", "x", "y"))
  expect_equal(comb2$cycles$uidx, 3L) # d's single file re-indexed to 3

  # a single object round-trips unchanged
  expect_equal(c(a), a)

  # only aggregated data can be combined
  c(a, 42) |> expect_error("must be.*ir_aggregated_data")
})
