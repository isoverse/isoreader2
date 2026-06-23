test_that("add_trace_timepoint_index() adds a per-series 1..n integer index", {
  # two trace series (channels) sharing a time grid, with rows shuffled so the
  # tp must be derived from time.s order, not row order
  traces <- tibble(
    species = "N2",
    channel = rep(c(1L, 2L), each = 3),
    mass = rep(c("28", "29"), each = 3),
    time.s = c(0, 2, 1, 1, 0, 2), # out of order within each channel
    intensity.V = c(10, 30, 20, 5, 1, 9)
  )
  out <- add_trace_timepoint_index(traces)

  # integer tp placed right before time.s
  expect_type(out$tp, "integer")
  expect_equal(which(names(out) == "tp"), which(names(out) == "time.s") - 1L)

  # tp is 1..n per series, ordered by time.s (row order is preserved)
  ch1 <- out[out$channel == 1L, ]
  expect_equal(ch1$tp, c(1L, 3L, 2L)) # time.s 0,2,1 -> ranks 1,3,2
  expect_equal(ch1$tp[order(ch1$time.s)], 1:3)
  ch2 <- out[out$channel == 2L, ]
  expect_equal(sort(ch2$tp), 1:3)

  # not a traces tibble -> returned unchanged
  expect_identical(add_trace_timepoint_index(tibble(x = 1)), tibble(x = 1))
  expect_null(add_trace_timepoint_index(NULL))
})

test_that("reading + aggregating adds an integer tp to traces only", {
  iso <- ir_examples_folder() |>
    ir_find_isofiles() |>
    ir_read_isofiles(show_progress = FALSE, show_problems = FALSE) |>
    suppressMessages()

  # nested traces carry an integer tp = 1..n per series; scans/cycles do not
  cf <- iso[grepl("continuous_flow_ea", iso$file_path), ]
  tr <- cf$traces[[1]]
  expect_type(tr$tp, "integer")
  one <- tr[tr$mass == tr$mass[1], ]
  expect_equal(one$tp[order(one$time.s)], seq_len(nrow(one)))

  scn <- iso[grepl("full_scan", iso$file_path), ]$scans[[1]]
  expect_false("tp" %in% names(scn))

  # aggregation includes tp (integer) for traces, but not scans/cycles
  agg <- iso |> ir_aggregate_isofiles("mV") |> suppressMessages()
  expect_type(agg$traces$tp, "integer")
  expect_false("tp" %in% names(agg$scans))
  expect_false("tp" %in% names(agg$cycles))
})

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
