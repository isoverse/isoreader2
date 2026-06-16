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

test_that("plotting functions filter by species and mass", {
  d <- tibble(
    file_name = "a",
    species = c("N2", "N2", "CO2", "CO2"),
    mass = c(28, 29, 44, 45),
    trace = paste0(species, ": ", mass),
    time.s = 0,
    intensity.mV = 1:4
  )

  species_in <- function(p) {
    sort(unique(as.character(ggplot2::ggplot_build(p)$plot$data$species)))
  }
  mass_in <- function(p) {
    sort(unique(as.character(ggplot2::ggplot_build(p)$plot$data$mass)))
  }

  # species filter
  expect_equal(species_in(ir_plot_continuous_flow(d)), c("CO2", "N2"))
  expect_equal(species_in(ir_plot_continuous_flow(d, species = "CO2")), "CO2")
  # mass filter (numeric value matches the character mass column)
  expect_equal(mass_in(ir_plot_continuous_flow(d, mass = 44)), "44")
  # combined
  p <- ir_plot_continuous_flow(d, species = "N2", mass = c(28, 29))
  expect_equal(species_in(p), "N2")
  expect_equal(mass_in(p), c("28", "29"))

  # informative errors when a selection leaves no data
  ir_plot_continuous_flow(d, species = "Ar") |>
    expect_error("no data left.*species")
  ir_plot_continuous_flow(d, mass = 99) |>
    expect_error("no data left.*mass")

  # the same parameters work for scans and dual inlet
  sc <- dplyr::mutate(
    d,
    scan_type = "high voltage",
    x_units = "kV",
    x = 0
  )
  expect_equal(species_in(ir_plot_scans(sc, species = "CO2")), "CO2")
  di <- dplyr::mutate(d, type = "sample", cycle = 1L)
  expect_equal(mass_in(ir_plot_dual_inlet(di, mass = 28)), "28")
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

test_that("ir_plot_dual_inlet() respects cycle_window", {
  d <- tibble(
    file_name = "a",
    species = "CO2",
    mass = 44,
    trace = "CO2: 44",
    type = "sample",
    cycle = 1:6,
    intensity.mV = c(1, 2, 3, 4, 5, 6)
  )

  p <- ir_plot_dual_inlet(d, cycle_window = c(2, 4))
  expect_no_error(ggplot2::ggplot_build(p))
  # display is clipped to the window
  expect_equal(p$coordinates$limits$x, c(2, 4))
  # data just outside the window is retained for edge autoscaling (cycles 1-5)
  expect_equal(sort(unique(p$data$cycle)), 1:5)

  # the value is validated as a length-2 numeric with min < max
  ir_plot_dual_inlet(d, cycle_window = 5) |>
    expect_error("numeric vector of length 2")
  ir_plot_dual_inlet(d, cycle_window = c(4, 2)) |>
    expect_error("min < max")
})

test_that("windows without data points of their own are allowed", {
  # a continuous flow trace with gaps between the data points
  cf <- tibble(
    file_name = "a",
    species = "CO2",
    mass = 44,
    trace = "CO2: 44",
    time.s = c(0, 10, 20, 30, 40),
    intensity.mV = c(1, 2, 3, 4, 5)
  )

  # a window that falls *between* two data points still plots: the bracketing
  # points on either side are kept so the line interpolates across the window
  p <- ir_plot_continuous_flow(cf, time_window = c(12, 18)) |>
    suppressMessages()
  expect_no_error(ggplot2::ggplot_build(p))
  expect_equal(sort(p$data$time.s), c(10, 20))
  expect_equal(p$coordinates$limits$x, c(12, 18))

  # a window beyond the data range keeps the nearest bracketing point, no error
  p_past <- ir_plot_continuous_flow(cf, time_window = c(100, 200)) |>
    suppressMessages()
  expect_no_error(ggplot2::ggplot_build(p_past))
  expect_equal(p_past$data$time.s, 40)

  # an impossible window (min >= max) is the only window error
  ir_plot_continuous_flow(cf, time_window = c(20, 10)) |>
    expect_error("min < max")
  ir_plot_continuous_flow(cf, time_window = c(20, 20)) |>
    expect_error("min < max")

  # same empty-window tolerance for scans and dual inlet
  sc <- tibble(
    file_name = "a",
    species = "CO2",
    mass = 44,
    trace = "CO2: 44",
    scan_type = "high voltage",
    x_units = "kV",
    x = c(0, 1, 2, 3),
    intensity.mV = c(1, 2, 3, 4)
  )
  p_sc <- ir_plot_scans(sc, x_window = c(1.2, 1.8)) |> suppressMessages()
  expect_no_error(ggplot2::ggplot_build(p_sc))
  expect_equal(sort(p_sc$data$x), c(1, 2))

  di <- tibble(
    file_name = "a",
    species = "CO2",
    mass = 44,
    trace = "CO2: 44",
    type = "sample",
    cycle = 1:6,
    intensity.mV = 1:6
  )
  p_di <- ir_plot_dual_inlet(di, cycle_window = c(2.4, 2.6)) |>
    suppressMessages()
  expect_no_error(ggplot2::ggplot_build(p_di))
  expect_equal(sort(p_di$data$cycle), c(2, 3))
})

test_that("y axis gets headroom on both ends inside a window", {
  y_range <- function(p) {
    ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y.range
  }

  # continuous flow: data well above 0 so we can see whether 0 is forced in
  cf <- tibble(
    file_name = "a",
    species = "CO2",
    mass = 44,
    trace = "CO2: 44",
    time.s = 0:10,
    intensity.mV = 5:15
  )
  # no window: 0 is included and pinned to the bottom of the panel
  expect_equal(y_range(ir_plot_continuous_flow(cf) |> suppressMessages())[1], 0)
  # window: data is bracketed to [7, 13]; both ends gain headroom (no touching)
  yw <- y_range(
    ir_plot_continuous_flow(cf, time_window = c(3, 7)) |> suppressMessages()
  )
  expect_gt(yw[1], 0)
  expect_lt(yw[1], 7)
  expect_gt(yw[2], 13)

  # dual inlet behaves the same
  di <- tibble(
    file_name = "a",
    species = "CO2",
    mass = 44,
    trace = "CO2: 44",
    type = "sample",
    cycle = 1:6,
    intensity.mV = 10:15
  )
  expect_equal(y_range(ir_plot_dual_inlet(di) |> suppressMessages())[1], 0)
  ydw <- y_range(
    ir_plot_dual_inlet(di, cycle_window = c(2, 4)) |> suppressMessages()
  )
  expect_gt(ydw[1], 0)
  expect_lt(ydw[1], 11)
  expect_gt(ydw[2], 13)

  # scans: window gets headroom below the lowest displayed value too
  sc <- tibble(
    file_name = "a",
    species = "CO2",
    mass = 44,
    trace = "CO2: 44",
    scan_type = "high voltage",
    x_units = "kV",
    x = 0:10,
    intensity.mV = 5:15
  )
  ysw <- y_range(
    ir_plot_scans(sc, x_window = c(3, 7)) |> suppressMessages()
  )
  expect_gt(ysw[1], 0)
  expect_lt(ysw[1], 7)
  expect_gt(ysw[2], 13)
})

test_that("zooming preserves trace/mass factor levels and colours", {
  # two traces, the second only has data outside the zoom window
  d <- tibble(
    file_name = "a",
    species = "CO2",
    mass = rep(c(44, 45), each = 6),
    trace = rep(c("CO2: 44", "CO2: 45"), each = 6),
    time.s = rep(seq(0, 50, by = 10), 2),
    intensity.mV = c(1:6, rep(0, 5), 9) # mass 45 only rises at the last point
  )

  full <- ir_plot_continuous_flow(d) |> suppressMessages()
  zoom <- ir_plot_continuous_flow(d, time_window = c(0, 20)) |>
    suppressMessages()
  expect_no_error(ggplot2::ggplot_build(zoom))

  # the full set of trace levels survives the zoom (no dropped levels)
  expect_equal(levels(full$data$trace), c("CO2: 44", "CO2: 45"))
  expect_equal(levels(zoom$data$trace), levels(full$data$trace))

  # the colour scale keeps every level and maps them to identical colours, so the
  # remaining traces are not re-coloured when zoomed
  sf <- ggplot2::ggplot_build(full)$plot$scales$get_scales("colour")
  sz <- ggplot2::ggplot_build(zoom)$plot$scales$get_scales("colour")
  expect_equal(sz$get_breaks(), sf$get_breaks())
  expect_equal(sz$map(sz$get_breaks()), sf$map(sf$get_breaks()))
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
