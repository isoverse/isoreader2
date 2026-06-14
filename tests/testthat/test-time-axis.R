test_that("get_timebase() picks an appropriate unit", {
  # interval of 60 s -> minutes; 3600 -> hours; 1 -> seconds
  expect_equal(names(get_timebase(60, cutoff = 0.5)), "m")
  expect_equal(names(get_timebase(3600, cutoff = 0.5)), "h")
  expect_equal(names(get_timebase(1, cutoff = 0.5)), "s")
  # sub-second
  expect_equal(names(get_timebase(1e-3, cutoff = 0.5)), "ms")
  # extremely small falls back to the smallest unit
  expect_equal(names(get_timebase(1e-20, cutoff = 0.5)), "fs")
})

test_that("breaks_pretty_duration() returns sensible breaks", {
  brk <- breaks_pretty_duration(n = 5)
  expect_type(brk, "closure")
  b <- brk(c(0, 120))
  expect_true(0 %in% b && 120 %in% b)
  expect_true(all(diff(b) > 0))
})

test_that("labels_duration() formats durations", {
  expect_equal(labels_duration()(c(0, 60, 120)), c("0 min", "1 min", "2 min"))
  # hours
  expect_equal(labels_duration()(c(0, 3600)), c("0 hours", "1 hours"))
  # short format drops the space and abbreviates the unit
  expect_equal(
    labels_duration(short_format = TRUE)(c(0, 3600)),
    c("0hr", "1hr")
  )
  # sub-second units
  expect_equal(labels_duration()(c(0, 0.001, 0.002)), c("0 ms", "1 ms", "2 ms"))
  # NA passes through
  expect_true(is.na(labels_duration()(c(0, 60, NA))[3]))
})
