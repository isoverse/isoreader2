# minimal plot data frames with the columns each plotting function requires
cf_data <- function() {
  tibble(
    file_name = "a",
    species = "CO2",
    mass = 44,
    trace = "CO2: 44",
    time.s = c(0, 1, 2),
    intensity.mV = c(1, 2, 3)
  )
}
di_data <- function() {
  tibble(
    file_name = "a",
    species = "CO2",
    mass = 44,
    trace = "CO2: 44",
    type = "sample",
    cycle = 1:3,
    intensity.mV = c(1, 2, 3)
  )
}
scn_data <- function() {
  tibble(
    file_name = "a",
    species = "CO2",
    mass = 44,
    trace = "CO2: 44",
    scan_type = "high voltage",
    x_units = "kV",
    x = c(0, 1, 2),
    intensity.mV = c(1, 2, 3)
  )
}

test_that("ir_default_theme()", {
  expect_s3_class(ir_default_theme(), "theme")
  expect_error(ir_default_theme(text_size = "big"), "must be a single number")
  # facet_text_size is NULL by default: strip text keeps its inherited
  # (relative) size; supplying a number sets an absolute override
  expect_s3_class(ir_default_theme()$strip.text$size, "rel")
  expect_equal(ir_default_theme(facet_text_size = 20)$strip.text$size, 20)
  expect_error(
    ir_default_theme(facet_text_size = c(1, 2)),
    "must be NULL or a single number"
  )
})

test_that("plotting functions reject raw isofiles and invalid input", {
  iso <- structure(
    tibble(file_path = "a.dxf", metadata = list(tibble())),
    class = c("ir_isofiles", "tbl_df", "tbl", "data.frame")
  )
  ir_plot_continuous_flow(iso) |> expect_error("aggregate")
  ir_plot_dual_inlet(iso) |> expect_error("aggregate")
  ir_plot_scans(iso) |> expect_error("aggregate")

  ir_plot_continuous_flow(42) |> expect_error("must be a data frame")
  ir_plot_dual_inlet(42) |> expect_error("must be a data frame")
  ir_plot_scans(42) |> expect_error("must be a data frame")
})

test_that("plotting functions error on missing required columns", {
  ir_plot_continuous_flow(cf_data() |> dplyr::select(-"time.s")) |>
    expect_error("missing required")
  ir_plot_dual_inlet(di_data() |> dplyr::select(-"cycle")) |>
    expect_error("missing required")
  ir_plot_scans(scn_data() |> dplyr::select(-"x")) |>
    expect_error("missing required")
})

test_that("ir_plot_continuous_flow() builds a ggplot", {
  p <- ir_plot_continuous_flow(cf_data())
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  # formula facet -> facet_grid; plain expression -> facet_wrap
  expect_s3_class(
    ir_plot_continuous_flow(cf_data(), facet = species ~ mass)$facet,
    "FacetGrid"
  )
  expect_s3_class(ir_plot_continuous_flow(cf_data())$facet, "FacetWrap")

  # nrow/ncol warn when used with a formula facet
  expect_warning(
    ir_plot_continuous_flow(cf_data(), facet = species ~ mass, nrow = 2),
    "only apply"
  )
})

test_that("ir_plot_dual_inlet() builds a ggplot", {
  p <- ir_plot_dual_inlet(di_data())
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("ir_plot_scans() builds a ggplot and handles scan_type", {
  p <- ir_plot_scans(scn_data())
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  # multiple scan types require an explicit scan_type
  multi <- dplyr::bind_rows(
    scn_data(),
    scn_data() |> dplyr::mutate(scan_type = "magnet current")
  )
  ir_plot_scans(multi) |> expect_error("scan type")
  expect_s3_class(
    ir_plot_scans(multi, scan_type = "high voltage"),
    "ggplot"
  )
})
