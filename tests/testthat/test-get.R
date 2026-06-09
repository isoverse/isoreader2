test_that("ir_get_data()", {
  fake_agg <- structure(
    list(traces = tibble(), cycles = tibble(), scans = tibble()),
    class = "ir_aggregated_data"
  )
  ir_get_data(fake_agg, traces = everything(), cycles = everything()) |>
    expect_error("only one of.*traces.*cycles.*scans")
  ir_get_data(fake_agg, traces = everything(), scans = everything()) |>
    expect_error("only one of.*traces.*cycles.*scans")
  ir_get_data(fake_agg, cycles = everything(), scans = everything()) |>
    expect_error("only one of.*traces.*cycles.*scans")
})

test_that("check_aggregated_dataset()", {
  # not an aggregated dataset
  check_aggregated_dataset(c("a.dxf", "b.dxf"), "scans") |>
    expect_error("must be a set of aggregated isofiles")

  # dataset not present (or not a data frame)
  agg <- structure(
    list(
      metadata = tibble(uidx = 1:2, file_name = c("a", "b")),
      scans = NULL
    ),
    class = "ir_aggregated_data"
  )
  check_aggregated_dataset(agg, "resistors") |>
    expect_error("does not include a.*resistors.*dataset.*extended")
  check_aggregated_dataset(agg, "scans") |>
    expect_error("does not include a.*scans.*dataset.*extended")

  # dataset present -> no error
  check_aggregated_dataset(agg, "metadata") |>
    expect_no_error()
})

test_that("ir_get_*() shortcuts", {
  agg <- structure(
    list(
      metadata = tibble(uidx = 1:2, file_name = c("a", "b"), date = c(1, 2)),
      resistors = tibble(uidx = c(1L, 1L, 2L), mass = c(44, 45, 44), Ohm = 1:3),
      traces = tibble(uidx = c(1L, 2L), time = c(0, 0), v44 = c(10, 30)),
      cycles = tibble(uidx = c(1L, 2L), d13C = c(-25, -10)),
      scans = tibble(uidx = c(1L, 2L), config = c("a", "b"), x = c(5, 6))
    ),
    class = "ir_aggregated_data"
  )

  # missing dataset errors (delegated to check_aggregated_dataset())
  empty <- structure(
    list(metadata = tibble(uidx = 1:2, file_name = c("a", "b"))),
    class = "ir_aggregated_data"
  )
  ir_get_resistors(empty) |> expect_error("does not include a.*resistors")
  ir_get_traces(empty) |> expect_error("does not include a.*traces")
  ir_get_cycles(empty) |> expect_error("does not include a.*cycles")
  ir_get_scans(empty) |> expect_error("does not include a.*scans")
  ir_get_metadata(c("a.dxf")) |>
    expect_error("must be a set of aggregated isofiles")
  ir_get_scans() |>
    expect_error("must be a set of aggregated isofiles.*not missing")

  # metadata: all columns
  expect_equal(
    ir_get_metadata(agg) |> suppressMessages(),
    agg$metadata
  )

  # resistors: keyed by file_name via metadata
  expect_equal(
    ir_get_resistors(agg) |> suppressMessages() |> names(),
    c("uidx", "file_name", "mass", "Ohm")
  )

  # traces / cycles join metadata file_name by uidx
  expect_equal(
    ir_get_traces(agg) |> suppressMessages() |> names(),
    c("uidx", "file_name", "time", "v44")
  )
  expect_equal(
    ir_get_cycles(agg) |> suppressMessages() |> names(),
    c("uidx", "file_name", "d13C")
  )

  # scans join metadata file_name by uidx + config
  expect_equal(
    ir_get_scans(agg) |> suppressMessages() |> names(),
    c("uidx", "file_name", "config", "x")
  )
})

test_that("get_data()", {
  # errors
  get_data() |> expect_error("must be.*list")
  get_data(42) |> expect_error("must be.*list")
  get_data(list()) |> expect_error("at least one")
  get_data(list(a = tibble()), by = 42) |> expect_error("must be.*character")
  get_data(list(a = tibble())) |> expect_error("no.*selections")
  get_data(list(a = tibble()), b = "a") |>
    expect_error("dataset.*not in the data")
  get_data(list(a = tibble(), d = tibble()), a = "a") |>
    expect_error("error selecting columns")
  get_data(list(a = cars, b = cars), a = "speed", b = "dist") |>
    expect_error("unclear how to join")
  get_data(
    list(a = cars, b = cars |> dplyr::mutate(speed = as.character(speed))),
    a = "speed",
    b = "dist",
    by = "speed"
  ) |>
    expect_error("encountered issue when joining")
  get_data(
    list(a = cars, b = cars),
    a = everything(),
    b = everything(),
    by = "speed"
  ) |>
    expect_error("encountered issue") # many-to-many relationship

  # working snapshots

  test_run1 <- function() {
    list(
      a = tibble(id = c("a", "b"), info = paste(id, "info")),
      b = tibble(id = "a", x = 1:10, y = 42),
      data = tibble(id = "a", x = 1:10, z = x * 10)
    ) |>
      get_data(
        a = everything(),
        b = c("id", "x"),
        data = everything(),
        by = c("id", "x")
      )
  }

  test_run2 <- function() {
    get_data(
      list(a = cars, b = cars),
      a = everything(),
      b = everything(),
      by = "speed",
      relationship = "many-to-many"
    )
  }

  # messages
  test_that_cli("cli", configs = c("plain", "fancy"), {
    expect_snapshot(out <- test_run1())
    expect_snapshot(out <- test_run2())
  }) |>
    withr::with_options(new = list(show_exec_times = FALSE))

  # data
  expect_snapshot_value(test_run1(), style = "json2") |>
    suppressMessages()
  expect_snapshot_value(test_run2(), style = "json2") |>
    suppressMessages()
})
