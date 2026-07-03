test_that("print.ir_aggregator() / knit_print run without error", {
  agg <- ir_get_aggregator("standard")
  expect_no_error(suppressMessages(print(agg)))
  expect_no_error(suppressMessages(knitr::knit_print(agg)))

  # an empty (freshly started) aggregator also prints
  expect_no_error(suppressMessages(print(ir_start_aggregator("empty"))))
})

test_that("get_cycle_info() lists each mass only once (not per cycle/type)", {
  # dual inlet cycles repeat every mass for each cycle and each standard/sample
  cycles <- tibble(
    species = "CO2",
    cycle = rep(1:2, each = 4),
    type = rep(c("standard", "sample"), each = 2, times = 2),
    mass = rep(c("44", "45"), times = 4)
  )
  info <- get_cycle_info(cycles) |> cli::ansi_strip()
  expect_match(info, "2 sample/standard cycles for CO2")
  # each mass appears exactly once (would be four times each without dedup)
  expect_length(gregexpr("44", info)[[1]], 1L)
  expect_length(gregexpr("45", info)[[1]], 1L)
})

test_that("print.ir_isofiles() handles an empty collection", {
  empty <- structure(
    tibble(file_path = character(0), metadata = list()),
    class = c("ir_isofiles", "tbl_df", "tbl", "data.frame")
  )
  expect_no_error(suppressMessages(print(empty)))
})

test_that("print methods run on real read + aggregated data", {
  skip_on_cran()
  skip_if(
    !file.exists(get_isoextract_path()),
    "isoextract binary is not installed"
  )

  data_dir <- withr::local_tempdir()
  file.copy(
    file.path(ir_examples_folder(), "continuous_flow_ea_example.dxf"),
    data_dir
  )
  iso <- data_dir |>
    ir_find_continuous_flow() |>
    ir_read_isofiles(show_progress = FALSE, show_problems = FALSE) |>
    suppressMessages()

  # ir_isofiles print + knit_print
  expect_no_error(suppressMessages(print(iso)))
  expect_no_error(suppressMessages(knitr::knit_print(iso)))

  # ir_aggregated_data print + knit_print
  agg <- iso |> ir_aggregate_isofiles() |> suppressMessages()
  expect_no_error(suppressMessages(print(agg)))
  expect_no_error(suppressMessages(knitr::knit_print(agg)))
})
