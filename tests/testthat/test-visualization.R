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
    expect_error("not a valid mass selection")

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

  # formula facet -> facet_grid; plain expression -> facet_wrap; NULL -> none
  expect_s3_class(
    ir_plot_continuous_flow(cf_data(), facet = species ~ mass)$facet,
    "FacetGrid"
  )
  expect_s3_class(
    ir_plot_continuous_flow(cf_data(), facet = file_name)$facet,
    "FacetWrap"
  )
  # default facet is NULL (no faceting for single-data-type data)
  expect_s3_class(ir_plot_continuous_flow(cf_data())$facet, "FacetNull")

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

test_that("add_trace_and_color_factors() keys the colour by species + mass", {
  levs <- c("N2: 28", "N2: 29", "N2: 29/28", "Ar: 36", "Ar: 40")
  d <- add_trace_and_color_factors(tibble(trace = levs))
  # every trace level gets a colour level, and an intensity trace shares one with
  # its ratio traces (keyed by species + numerator mass)
  expect_s3_class(d$color, "factor")
  expect_false(any(is.na(d$color)))
  expect_equal(
    as.character(d$color),
    c(
      "N2: 28",
      "N2: 29, 29/28",
      "N2: 29, 29/28",
      "Ar: 36",
      "Ar: 40"
    )
  )
  # the colour levels follow the trace order (species, then ascending mass)
  expect_equal(
    levels(d$trace),
    c("Ar: 36", "Ar: 40", "N2: 28", "N2: 29", "N2: 29/28")
  )
  expect_equal(
    levels(d$color),
    c("Ar: 36", "Ar: 40", "N2: 28", "N2: 29, 29/28")
  )

  # levels that have no data rows (as after a scan_type filter, which keeps all
  # levels but drops rows) still carry their full colour label
  kept <- add_trace_and_color_factors(
    tibble(trace = factor("N2: 28", levels = levs))
  )
  expect_equal(levels(kept$color), levels(d$color))

  # a trace label without a "<species>: " prefix falls back to the label itself
  expect_equal(
    levels(add_trace_and_color_factors(tibble(trace = c("b", "a")))$color),
    c("a", "b")
  )
  # ... and still keys an intensity trace to its ratios (an NA species)
  nas <- add_trace_and_color_factors(
    tibble(trace = c("28", "29", "29/28", "30", "30/28"))
  )
  expect_equal(levels(nas$color), c("28", "29, 29/28", "30, 30/28"))
  expect_equal(
    as.character(nas$color),
    c("28", "29, 29/28", "29, 29/28", "30, 30/28", "30, 30/28")
  )
  # data without a trace column is passed through untouched
  expect_identical(
    add_trace_and_color_factors(tibble(x = 1)),
    tibble(x = 1)
  )
})

test_that("an NA species drops the prefix from the trace and colour labels", {
  d <- function(species) {
    tibble(
      file_name = "a",
      species = species,
      mass = rep(c("28", "29"), each = 2),
      time.s = rep(c(0, 10), 2),
      intensity.mV = c(100, 200, 40, 80),
      ratio_name = rep(c(NA, "29/28"), each = 2),
      ratio = c(NA, NA, 0.4, 0.4)
    )
  }

  # NA species -> bare mass/ratio labels, never "NA: 28"
  tb <- ir_generate_traces_tibble(d(NA_character_))
  expect_equal(levels(tb$trace), c("28", "29", "29/28"))
  expect_equal(levels(tb$color), c("28", "29, 29/28"))
  expect_false(any(grepl("NA", levels(tb$trace))))

  # a named species is unaffected
  expect_equal(
    levels(ir_generate_traces_tibble(d("N2"))$color),
    c("N2: 28", "N2: 29, 29/28")
  )

  # mixed data keeps both forms, NA species first
  tb_mix <- ir_generate_traces_tibble(
    dplyr::bind_rows(d(NA_character_), d("N2"))
  )
  expect_equal(
    levels(tb_mix$color),
    c("28", "29, 29/28", "N2: 28", "N2: 29, 29/28")
  )

  # masses still sort ascending when there is no species to sort by first
  unsorted <- tibble(
    file_name = "a",
    species = NA_character_,
    mass = as.character(c(46, 44, 45)),
    time.s = 0,
    intensity.mV = 1:3
  )
  expect_equal(
    levels(ir_generate_traces_tibble(unsorted)$trace),
    c("44", "45", "46")
  )

  # the plot builds: 3 lines sharing 2 colours
  p <- ir_plot_continuous_flow(d(NA_character_)) |> suppressMessages()
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(dplyr::n_distinct(built$group), 3L)
  expect_equal(dplyr::n_distinct(built$colour), 2L)
})

test_that("every colour level gets a colour, even ones with no data rows", {
  # regression: more colour levels than the palette, some without data rows, must
  # still all end up with a distinct colour in the legend
  d <- tibble(
    file_name = "a",
    species = "CO2",
    mass = as.character(40:51), # 12 > 9 palette colours
    time.s = 0,
    intensity.mV = 1:12
  )
  p <- ir_plot_continuous_flow(d) |> suppressMessages()
  sc <- ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")
  cols <- sc$map(sc$get_breaks())
  expect_length(cols, 12)
  expect_false(any(is.na(cols)))
  expect_length(unique(cols), 12)
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

test_that("ir_plot_scans() drops traces belonging to other scan types", {
  # two scan types with different masses; the trace factor is built across both,
  # but after selecting one scan type only its traces should remain (no empty,
  # uncoloured legend entries for the other scan type's masses)
  sc <- dplyr::bind_rows(
    tibble(
      file_name = "a",
      species = "CO2",
      mass = c("44", "45"),
      scan_type = "high voltage",
      x_units = "kV",
      x = 0,
      intensity.mV = c(1, 2)
    ),
    tibble(
      file_name = "a",
      species = "Ar",
      mass = c("36", "40"),
      scan_type = "magnet current",
      x_units = "A",
      x = 0,
      intensity.mV = c(3, 4)
    )
  )
  p <- ir_plot_scans(sc, scan_type = "high voltage")
  b <- ggplot2::ggplot_build(p)
  # only the high-voltage traces survive as factor levels (Ar masses dropped)
  expect_equal(sort(levels(b$plot$data$trace)), c("CO2: 44", "CO2: 45"))
  # and the legend has exactly those two, each with a colour
  gd <- ggplot2::get_guide_data(p, "colour")
  expect_equal(nrow(gd), 2L)
  expect_false(any(is.na(gd$colour)))
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

test_that("ir_generate_*_tibble() add trace, color, data_type and value", {
  cols <- c("trace", "color", "data_type", "value")
  tb <- ir_generate_traces_tibble(cf_data())
  expect_true(all(cols %in% names(tb)))
  expect_s3_class(tb$trace, "factor")
  expect_s3_class(tb$color, "factor")
  expect_equal(as.character(unique(tb$trace)), "CO2: 44")
  # a trace with no ratios is its own colour group
  expect_equal(as.character(unique(tb$color)), "CO2: 44")
  expect_equal(unique(tb$data_type), "intensity [mV]")
  expect_equal(tb$value, c(1, 2, 3))

  # a ratio shares its colour group with its numerator mass
  tb_r <- ir_generate_traces_tibble(cf_ratio_data())
  expect_equal(
    levels(tb_r$color),
    c("N2: 28", "N2: 29, 29/28", "N2: 30, 30/28")
  )

  # cycles and scans variants work too
  expect_true(all(cols %in% names(ir_generate_cycles_tibble(di_data()))))
  expect_true(all(cols %in% names(ir_generate_scans_tibble(scn_data()))))
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

test_that("ratio columns that hold no ratios at all still plot", {
  # a species with a single mass: ir_calculate_ratios() adds the ratio columns,
  # but the only row is the base mass, so every ratio_name is NA and there are no
  # ratio rows to build. make_trace_label() must stay character-typed on that
  # zero-length input - ifelse() returns logical(0), which used to poison the
  # `trace` column and make the intensity/ratio bind_rows() fail with
  # "Can't combine `..1$trace` <character> and `..2$trace` <logical>"
  no_ratios <- tibble(
    file_name = "a",
    species = "N2",
    mass = "28",
    time.s = c(0, 1),
    intensity.mV = c(100, 120),
    ratio_name = NA_character_,
    ratio = NA_real_
  )

  tb <- ir_generate_traces_tibble(no_ratios)
  # just the intensity trace, no ratio rows
  expect_equal(nrow(tb), 2L)
  expect_type(as.character(tb$trace), "character")
  expect_equal(levels(tb$trace), "N2: 28")
  expect_equal(levels(tb$color), "N2: 28")
  expect_setequal(tb$data_type, "intensity [mV]")
  expect_equal(tb$value, c(100, 120))

  # and the plotting function gets all the way through
  expect_no_error(suppressMessages(
    ggplot2::ggplot_build(ir_plot_continuous_flow(no_ratios))
  ))
})

test_that("make_trace_label() is type stable", {
  # zero-length in, zero-length character out (never logical(0))
  expect_identical(make_trace_label(character(0), character(0)), character(0))
  expect_identical(
    make_trace_label(NA_character_[0], character(0)),
    character(0)
  )
  # the ordinary labels are unchanged
  expect_identical(
    make_trace_label(c("CO2", "N2"), c("44", "29/28")),
    c("CO2: 44", "N2: 29/28")
  )
  # an NA species drops the prefix rather than printing "NA: 44"
  expect_identical(
    make_trace_label(c(NA, "CO2"), c("44", "45")),
    c("44", "CO2: 45")
  )
  # a factor mass is labelled by its value, not its integer code
  expect_identical(make_trace_label("CO2", factor("44")), "CO2: 44")
})

test_that("asking for no ratios never needs the ratio columns", {
  # `c()` is documented to mean "none", exactly like NULL - neither is a request
  # for ratios, so neither may demand the ratio columns. Checking only for a
  # literal NULL rejected `c()` (and anything else evaluating to NULL) with a
  # "calculate ratios first" error, which also caught callers forwarding the
  # argument on with {{ }}
  no_ratio_cols <- cf_data()
  expect_false(all(c("ratio_name", "ratio") %in% names(no_ratio_cols)))

  none <- NULL
  for (sel in list(quote(NULL), quote(c()), quote(none))) {
    tb <- rlang::eval_tidy(rlang::expr(
      ir_generate_traces_tibble(no_ratio_cols, ratio = !!sel)
    ))
    expect_equal(nrow(tb), nrow(no_ratio_cols))
    expect_setequal(tb$data_type, "intensity [mV]")
    expect_no_error(suppressMessages(rlang::eval_tidy(rlang::expr(
      ggplot2::ggplot_build(ir_plot_continuous_flow(
        no_ratio_cols,
        ratio = !!sel
      ))
    ))))
  }
  # and it still selects nothing when the columns ARE there
  expect_setequal(
    ir_generate_traces_tibble(cf_ratio_data(), ratio = c())$data_type,
    "intensity [mV]"
  )
})

test_that("requesting ratios errors when not calculated or not present", {
  # ratio columns absent -> point at ir_calculate_ratios()
  ir_generate_traces_tibble(cf_data(), ratio = "45/44") |>
    expect_error("ir_calculate_ratios")
  # ratio columns present but the requested name is absent
  ir_generate_traces_tibble(cf_ratio_data(), ratio = "99/98") |>
    expect_error("not a valid ratio selection")
  # the plotting function surfaces the same error
  ir_plot_continuous_flow(cf_data(), ratio = "45/44") |>
    expect_error("ir_calculate_ratios")
})

test_that("the default colour groups traces by species + (numerator) mass", {
  p <- ir_plot_continuous_flow(
    cf_ratio_data(),
    ratio = c("29/28", "30/28")
  ) |>
    suppressMessages()

  # one colour level per species/mass, listing all of its traces, in mass order
  expect_equal(
    levels(p$data$color),
    c("N2: 28", "N2: 29, 29/28", "N2: 30, 30/28")
  )
  # each trace is mapped to its colour level
  trace_to_color <- dplyr::distinct(dplyr::select(p$data, "trace", "color"))
  expect_equal(
    as.character(trace_to_color$color[trace_to_color$trace == "N2: 29"]),
    as.character(trace_to_color$color[trace_to_color$trace == "N2: 29/28"])
  )
  # ... but the lines stay grouped by trace, so all 5 are drawn separately
  built <- ggplot2::ggplot_build(p)$data[[1]]
  expect_equal(dplyr::n_distinct(built$group), 5L)
  expect_equal(dplyr::n_distinct(built$colour), 3L)

  # the legend has one entry per colour level, all distinct, titled "trace"
  sc <- ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")
  expect_equal(
    as.character(sc$get_breaks()),
    c("N2: 28", "N2: 29, 29/28", "N2: 30, 30/28")
  )
  expect_length(unique(sc$map(sc$get_breaks())), 3L)
  expect_equal(ggplot2::get_labs(p)$colour, "trace")
})

test_that("the colour aesthetic can be overridden per trace", {
  p <- ir_plot_continuous_flow(
    cf_ratio_data(),
    ratio = c("29/28", "30/28"),
    color = trace
  ) |>
    suppressMessages()
  sc <- ggplot2::ggplot_build(p)$plot$scales$get_scales("colour")
  # every trace now gets its own colour and the legend is titled from the column
  expect_equal(
    as.character(sc$get_breaks()),
    c("N2: 28", "N2: 29", "N2: 29/28", "N2: 30", "N2: 30/28")
  )
  expect_length(unique(sc$map(sc$get_breaks())), 5L)
  expect_equal(ggplot2::get_labs(p)$colour, "trace")
  # an unrelated column keeps its own legend title
  p2 <- ir_plot_continuous_flow(cf_ratio_data(), color = mass) |>
    suppressMessages()
  expect_equal(ggplot2::get_labs(p2)$colour, "mass")
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

test_that("everything() shows all ratios, NULL shows none", {
  # everything() (the default) includes every available ratio
  tb_all <- ir_generate_traces_tibble(cf_ratio_data())
  expect_setequal(
    unique(as.character(tb_all$trace[tb_all$data_type == "ratios"])),
    c("N2: 29/28", "N2: 30/28")
  )
  expect_equal(
    tb_all,
    ir_generate_traces_tibble(cf_ratio_data(), ratio = everything())
  )
  # NULL, c(), and character(0) all include no ratios
  for (none in list(NULL, c(), character(0))) {
    tb_none <- ir_generate_traces_tibble(cf_ratio_data(), ratio = !!none)
    expect_false("ratios" %in% tb_none$data_type)
  }
  # data without ratio columns + the default -> no ratios, no error
  expect_no_error(ir_generate_traces_tibble(cf_data()))
  expect_false("ratios" %in% ir_generate_traces_tibble(cf_data())$data_type)
  # ... and the same with an explicit NULL
  expect_no_error(ir_generate_traces_tibble(cf_data(), ratio = NULL))
})

test_that("mass/ratio accept the full tidyselect syntax", {
  traces <- function(...) {
    sort(as.character(unique(
      ir_generate_traces_tibble(cf_ratio_data(), ...)$trace
    )))
  }

  # names, as character or (via as.character) numeric - including a range
  expect_equal(
    traces(mass = c("28", "29"), ratio = NULL),
    c("N2: 28", "N2: 29")
  )
  expect_equal(traces(mass = c(28, 29), ratio = NULL), c("N2: 28", "N2: 29"))
  expect_equal(traces(mass = 28:29, ratio = NULL), c("N2: 28", "N2: 29"))
  expect_equal(traces(mass = 28, ratio = NULL), "N2: 28")
  # numbers select by NAME, not by position (28:29 is not the 28th-29th mass)
  expect_error(traces(mass = 1:2, ratio = NULL), "not a valid mass selection")

  # a variable holding the names works without all_of()
  wanted <- c(28, 30)
  expect_equal(traces(mass = wanted, ratio = NULL), c("N2: 28", "N2: 30"))

  # negative selections
  expect_equal(traces(mass = -"28", ratio = NULL), c("N2: 29", "N2: 30"))
  expect_equal(traces(mass = !"28", ratio = NULL), c("N2: 29", "N2: 30"))
  expect_equal(
    traces(mass = NULL, ratio = -"29/28"),
    "N2: 30/28"
  )

  # helpers
  expect_equal(
    traces(mass = starts_with("2"), ratio = NULL),
    c("N2: 28", "N2: 29")
  )
  expect_equal(traces(mass = matches("^30$"), ratio = NULL), "N2: 30")
  expect_equal(traces(mass = NULL, ratio = starts_with("29")), "N2: 29/28")
  # all_of() is strict, any_of() ignores what is missing
  expect_error(
    traces(mass = all_of(c("28", "99")), ratio = NULL),
    "not a valid mass"
  )
  expect_equal(traces(mass = any_of(c("28", "99")), ratio = NULL), "N2: 28")

  # the error lists what is available, using the expression as written
  err <- tryCatch(traces(mass = 99), error = function(e) conditionMessage(e))
  expect_match(err, "mass.*=.*99.*is not a valid mass selection")
  expect_match(err, "available masses")
  expect_match(err, "28")
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

test_that("mass = NULL drops intensities (e.g. to plot only ratios)", {
  tb <- ir_generate_traces_tibble(cf_ratio_data(), mass = NULL)
  expect_true(all(tb$data_type == "ratios"))
  # dropping both masses and ratios leaves nothing -> error
  expect_error(
    ir_generate_traces_tibble(cf_ratio_data(), mass = NULL, ratio = NULL),
    "no data to plot"
  )
  expect_error(
    ir_generate_traces_tibble(cf_ratio_data(), mass = c(), ratio = c()),
    "no data to plot"
  )
})

test_that("the default facet is NULL (single data type -> no faceting)", {
  # cf_data has only intensities -> data_type not used as a facet row, and the
  # default facet = NULL -> no faceting at all
  p <- ir_plot_continuous_flow(cf_data())
  expect_s3_class(p$facet, "FacetNull")
  # y label shows the (single) data type
  expect_equal(p$labels$y, "intensity [mV]")
})

test_that("data_type_as_facet = auto() uses data_type when >1 type is present", {
  fvars <- function(p) {
    list(
      rows = names(p$facet$params$rows),
      cols = names(p$facet$params$cols)
    )
  }
  # cf_ratio_data has both intensities and ratios -> auto() faceting on data_type;
  # the default facet = NULL gives data_type ~ .
  p <- ir_plot_continuous_flow(cf_ratio_data()) |> suppressMessages()
  expect_s3_class(p$facet, "FacetGrid")
  expect_equal(fvars(p)$rows, "data_type")
  expect_length(fvars(p)$cols, 0)
  expect_null(p$labels$y) # the strip provides the label

  # a single facet variable becomes the column: data_type ~ file_name
  p_col <- ir_plot_continuous_flow(cf_ratio_data(), facet = file_name) |>
    suppressMessages()
  expect_s3_class(p_col$facet, "FacetGrid")
  expect_equal(fvars(p_col)$rows, "data_type")
  expect_equal(fvars(p_col)$cols, "file_name")
})

test_that("the data_type facet strip sits on the left, outside the y axis", {
  # data_type used as a facet row -> row strips switched to the left and placed
  # outside the y axis (next to the per-row y-axis values)
  p <- ir_plot_continuous_flow(cf_ratio_data()) |> suppressMessages()
  expect_equal(p$facet$params$switch, "y")
  expect_equal(p$theme$strip.placement, "outside")

  # not applied when data_type is not used as a facet row
  p_no <- ir_plot_continuous_flow(
    cf_ratio_data(),
    data_type_as_facet = FALSE
  ) |>
    suppressMessages()
  expect_false(identical(p_no$facet$params$switch, "y"))
  expect_false(identical(p_no$theme$strip.placement, "outside"))
})

test_that("data_type_as_facet TRUE/FALSE override the automatic choice", {
  # FALSE with two data types: no data_type facet row; with the default facet
  # (NULL) that means no faceting, and the y label shows both data types
  p_false <- ir_plot_continuous_flow(
    cf_ratio_data(),
    data_type_as_facet = FALSE
  ) |>
    suppressMessages()
  expect_s3_class(p_false$facet, "FacetNull")
  expect_equal(p_false$labels$y, "intensity [mV] / ratios")

  # TRUE with a single data type: forces the data_type facet row; y dropped
  p_true <- ir_plot_continuous_flow(cf_data(), data_type_as_facet = TRUE)
  expect_s3_class(p_true$facet, "FacetGrid")
  expect_equal(names(p_true$facet$params$rows), "data_type")
  expect_null(p_true$labels$y)

  # validated
  expect_error(
    ir_plot_continuous_flow(cf_data(), data_type_as_facet = "yes"),
    "auto\\(\\), TRUE, or FALSE"
  )
})

test_that("a formula facet ignores data_type_as_facet (with a warning if TRUE)", {
  # formula facet always wins; data_type is not used as a row, y shows the types
  p <- ir_plot_continuous_flow(cf_ratio_data(), facet = species ~ mass) |>
    suppressMessages()
  expect_s3_class(p$facet, "FacetGrid")
  expect_equal(names(p$facet$params$rows), "species")
  expect_equal(p$labels$y, "intensity [mV] / ratios")

  # explicitly combining TRUE with a formula warns (mutually exclusive)
  expect_warning(
    ir_plot_continuous_flow(
      cf_ratio_data(),
      facet = species ~ mass,
      data_type_as_facet = TRUE
    ) |>
      suppressMessages(),
    "mutually exclusive|ignored because"
  )
})
