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
  # the base text size is applied
  expect_equal(ir_default_theme(text_size = 20)$text$size, 20)
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
  p <- ir_plot_continuous_flow(cf, time_window.s = c(12, 18)) |>
    suppressMessages()
  expect_no_error(ggplot2::ggplot_build(p))
  expect_equal(sort(p$data$time.s), c(10, 20))
  expect_equal(p$coordinates$limits$x, c(12, 18))

  # a window beyond the data range keeps the nearest bracketing point, no error
  p_past <- ir_plot_continuous_flow(cf, time_window.s = c(100, 200)) |>
    suppressMessages()
  expect_no_error(ggplot2::ggplot_build(p_past))
  expect_equal(p_past$data$time.s, 40)

  # an impossible window (min >= max) is the only window error
  ir_plot_continuous_flow(cf, time_window.s = c(20, 10)) |>
    expect_error("min < max")
  ir_plot_continuous_flow(cf, time_window.s = c(20, 20)) |>
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

test_that("ir_plot_continuous_flow() accepts the time window in seconds or minutes", {
  cf <- tibble(
    file_name = "a",
    species = "CO2",
    mass = 44,
    trace = "CO2: 44",
    time.s = c(0, 60, 120, 180, 240),
    intensity.mV = c(1, 2, 3, 4, 5)
  )
  # 1-3 minutes == 60-180 seconds: identical clipping
  p_min <- ir_plot_continuous_flow(cf, time_window.min = c(1, 3)) |>
    suppressMessages()
  p_sec <- ir_plot_continuous_flow(cf, time_window.s = c(60, 180)) |>
    suppressMessages()
  expect_equal(p_min$coordinates$limits$x, c(60, 180))
  expect_equal(p_min$coordinates$limits$x, p_sec$coordinates$limits$x)
  expect_equal(sort(p_min$data$time.s), sort(p_sec$data$time.s))

  # both NULL (default) -> no window, all points shown
  p_none <- ir_plot_continuous_flow(cf)
  expect_null(p_none$coordinates$limits$x)
  expect_equal(sort(p_none$data$time.s), cf$time.s)

  # an invalid minutes window is validated too
  ir_plot_continuous_flow(cf, time_window.min = c(3, 1)) |>
    expect_error("min < max")
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
  zoom <- ir_plot_continuous_flow(d, time_window.s = c(0, 20)) |>
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

test_that("drop_unused_levels is validated and trace colouring scales past palette", {
  breaks <- function(p) {
    as.character(
      ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")$get_breaks()
    )
  }

  # the flag is validated
  expect_error(
    ir_plot_continuous_flow(cf_data(), drop_unused_levels = "yes"),
    "TRUE OR FALSE|TRUE or FALSE"
  )

  # more traces than the default colour palette still get one colour break per
  # trace (the trace colour path falls back to generated hues)
  many <- tibble(
    file_name = "a",
    species = "CO2",
    mass = as.character(40:54),
    time.s = 0,
    intensity.mV = 1:15
  )
  expect_length(breaks(ir_plot_continuous_flow(many) |> suppressMessages()), 15)
})

test_that("drop_unused_levels drops traces that are outside the zoom window", {
  breaks <- function(p) {
    as.character(
      ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")$get_breaks()
    )
  }

  d <- dplyr::bind_rows(
    # spans the window
    tibble(
      file_name = "a",
      species = "CO2",
      mass = "44",
      trace = "CO2: 44",
      time.s = 0:10,
      intensity.mV = 5:15
    ),
    # only after the window (kept only as an off-window bracketing point)
    tibble(
      file_name = "a",
      species = "CO2",
      mass = "45",
      trace = "CO2: 45",
      time.s = c(8, 9, 10),
      intensity.mV = 1:3
    ),
    # straddles the window: no in-window point but the line crosses it
    tibble(
      file_name = "a",
      species = "CO2",
      mass = "46",
      trace = "CO2: 46",
      time.s = c(0, 20),
      intensity.mV = c(100, 200)
    )
  )
  w <- c(3, 7)

  # default keeps every trace even though only the bracketing points of some
  # remain in the window (stable colour mapping)
  expect_equal(
    breaks(ir_plot_continuous_flow(d, time_window.s = w) |> suppressMessages()),
    c("CO2: 44", "CO2: 45", "CO2: 46")
  )
  # drop_unused_levels = TRUE drops the trace that is entirely outside the window
  # but keeps the one inside it and the one whose line crosses it
  expect_equal(
    breaks(
      ir_plot_continuous_flow(
        d,
        time_window.s = w,
        drop_unused_levels = TRUE
      ) |>
        suppressMessages()
    ),
    c("CO2: 44", "CO2: 46")
  )
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

# trace data with pre-computed ratios (as ir_calculate_ratios() would add them)
cf_ratio_data <- function() {
  tibble(
    file_name = "a",
    species = "N2",
    mass = c("28", "29", "30"),
    time.s = 0,
    intensity.mV = c(100, 40, 10),
    ratio_name = c(NA, "29/28", "30/28"),
    ratio = c(NA, 0.4, 0.1)
  )
}

test_that("ir_generate_*_tibble() add trace, data_type and value", {
  tb <- ir_generate_traces_tibble(cf_data())
  expect_true(all(c("trace", "data_type", "value") %in% names(tb)))
  expect_s3_class(tb$trace, "factor")
  expect_equal(as.character(unique(tb$trace)), "CO2: 44")
  expect_equal(unique(tb$data_type), "intensity [mV]")
  expect_equal(tb$value, c(1, 2, 3))

  # cycles and scans variants work too
  expect_true(all(
    c("trace", "data_type", "value") %in%
      names(ir_generate_cycles_tibble(di_data()))
  ))
  expect_true(all(
    c("trace", "data_type", "value") %in%
      names(ir_generate_scans_tibble(scn_data()))
  ))
})

test_that("trace is always (re)generated from species + mass", {
  # a bogus incoming trace column is overwritten
  d <- cf_data() |> dplyr::mutate(trace = "WRONG")
  expect_equal(
    as.character(unique(ir_generate_traces_tibble(d)$trace)),
    "CO2: 44"
  )
})

test_that("ir_generate_traces_tibble() includes requested ratios", {
  tb <- ir_generate_traces_tibble(cf_ratio_data(), ratio = "29/28")
  # 3 intensity rows + 1 ratio row
  expect_equal(nrow(tb), 4L)

  ratio_rows <- tb[tb$data_type == "ratios", ]
  expect_equal(nrow(ratio_rows), 1L)
  expect_equal(as.character(ratio_rows$trace), "N2: 29/28")
  expect_equal(ratio_rows$value, 0.4)

  int_rows <- tb[tb$data_type == "intensity [mV]", ]
  expect_setequal(as.character(int_rows$trace), c("N2: 28", "N2: 29", "N2: 30"))
  expect_setequal(int_rows$value, c(100, 40, 10))

  # the ratio trace sorts right after its intensity trace (species + numerator)
  expect_equal(
    levels(tb$trace),
    c("N2: 28", "N2: 29", "N2: 29/28", "N2: 30")
  )
})

test_that("requesting ratios errors when not calculated or not present", {
  # ratio columns absent -> point at ir_calculate_ratios()
  ir_generate_traces_tibble(cf_data(), ratio = "45/44") |>
    expect_error("ir_calculate_ratios")
  # ratio columns present but the requested name is absent
  ir_generate_traces_tibble(cf_ratio_data(), ratio = "99/98") |>
    expect_error("no data left after filtering ratios")
  # the plotting function surfaces the same error
  ir_plot_continuous_flow(cf_data(), ratio = "45/44") |>
    expect_error("ir_calculate_ratios")
})

test_that("colouring by trace shares a colour across species + (numerator) mass", {
  p <- ir_plot_continuous_flow(
    cf_ratio_data(),
    ratio = c("29/28", "30/28")
  ) |>
    suppressMessages()
  sc <- ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")
  cols <- stats::setNames(
    sc$map(sc$get_breaks()),
    as.character(sc$get_breaks())
  )

  # the intensity trace and the ratio trace with the same numerator mass match
  expect_equal(unname(cols["N2: 29"]), unname(cols["N2: 29/28"]))
  expect_equal(unname(cols["N2: 30"]), unname(cols["N2: 30/28"]))
  # different numerator masses get different colours
  expect_false(unname(cols["N2: 28"]) == unname(cols["N2: 29"]))
  expect_false(unname(cols["N2: 29"]) == unname(cols["N2: 30"]))
})

test_that("ratio rows carry a 'ratios' data_type and the plot uses value as y", {
  p <- ir_plot_continuous_flow(cf_ratio_data(), ratio = "29/28") |>
    suppressMessages()
  expect_no_error(ggplot2::ggplot_build(p))
  # both data types are present in the plotted data
  expect_setequal(
    unique(p$data$data_type),
    c("intensity [mV]", "ratios")
  )
  # the ratio value (0.4) is plotted as `value`
  expect_true(0.4 %in% p$data$value)
})

test_that("ratio = NULL shows all ratios, character(0) shows none", {
  # NULL (default) includes every available ratio
  tb_all <- ir_generate_traces_tibble(cf_ratio_data())
  expect_setequal(
    unique(as.character(tb_all$trace[tb_all$data_type == "ratios"])),
    c("N2: 29/28", "N2: 30/28")
  )
  # character(0) includes no ratios
  tb_none <- ir_generate_traces_tibble(cf_ratio_data(), ratio = character(0))
  expect_false("ratios" %in% tb_none$data_type)
  # data without ratio columns + ratio = NULL -> no ratios, no error
  expect_no_error(ir_generate_traces_tibble(cf_data()))
  expect_false("ratios" %in% ir_generate_traces_tibble(cf_data())$data_type)
})

test_that("a ratio plots even when its numerator mass is excluded by `mass`", {
  # mass = "28" excludes mass 29, but ratio "29/28" (numerator 29) must still plot
  # because the mass filter only drops intensity rows, never ratio rows
  tb <- ir_generate_traces_tibble(cf_ratio_data(), mass = "28", ratio = "29/28")
  expect_setequal(
    as.character(unique(tb$trace)),
    c("N2: 28", "N2: 29/28")
  )
  expect_true("ratios" %in% tb$data_type)
  expect_equal(tb$value[tb$data_type == "ratios"], 0.4)
  # the same holds in the plotting function
  p <- ir_plot_continuous_flow(
    cf_ratio_data(),
    mass = "28",
    ratio = "29/28"
  ) |>
    suppressMessages()
  expect_true(
    "N2: 29/28" %in% as.character(ggplot2::ggplot_build(p)$plot$data$trace)
  )
})

test_that("mass = character(0) drops intensities (e.g. to plot only ratios)", {
  tb <- ir_generate_traces_tibble(cf_ratio_data(), mass = character(0))
  expect_true(all(tb$data_type == "ratios"))
  # dropping both masses and ratios leaves nothing -> error
  expect_error(
    ir_generate_traces_tibble(
      cf_ratio_data(),
      mass = character(0),
      ratio = character(0)
    ),
    "no data to plot"
  )
})

test_that("the default facet is data_type", {
  p <- ir_plot_continuous_flow(cf_data())
  expect_s3_class(p$facet, "FacetWrap")
  expect_equal(names(p$facet$params$facets), "data_type")
})
