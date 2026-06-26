# a minimal ir_aggregated_data with a traces dataset: two species, each with a
# couple of masses, at two time points
agg_traces <- function() {
  structure(
    list(
      metadata = tibble(uidx = 1L, analysis = 1L, file_name = "a"),
      traces = tibble(
        uidx = 1L,
        analysis = 1L,
        species = rep(c("N2", "CO2"), each = 6),
        mass = c(
          "28",
          "29",
          "30",
          "28",
          "29",
          "30",
          "44",
          "45",
          "46",
          "44",
          "45",
          "46"
        ),
        time.s = rep(c(0, 1), each = 3) |> rep(2),
        intensity.mV = c(
          # N2 t=0: 28->100, 29->40, 30->10 ; t=1: 28->200, 29->100, 30->50
          100,
          40,
          10,
          200,
          100,
          50,
          # CO2 t=0: 44->1000, 45->100, 46->10 ; t=1: 44->500, 45->50, 46->5
          1000,
          100,
          10,
          500,
          50,
          5
        )
      )
    ),
    class = "ir_aggregated_data"
  )
}

test_that("ir_calculate_ratios() validates its input", {
  expect_error(ir_calculate_ratios(42), "aggregated isofiles")
  expect_error(
    ir_calculate_ratios(agg_traces(), 28),
    "must be named by species"
  )
  expect_error(
    ir_calculate_ratios(agg_traces(), N2 = "x"),
    "must be a single number"
  )
  # no traces/cycles/scans present
  md_only <- structure(
    list(metadata = tibble(uidx = 1L, file_name = "a")),
    class = "ir_aggregated_data"
  )
  expect_error(ir_calculate_ratios(md_only), "no .*data with intensities")
})

test_that("ir_calculate_ratios() adds ratio columns to the source series", {
  # num_add.V = 0 gives the plain intensity ratio (no additive offset)
  res <- ir_calculate_ratios(agg_traces(), num_add.V = 0) |>
    suppressMessages()
  expect_s3_class(res, "ir_aggregated_data")
  # no new datasets are created; columns are added to `traces` instead
  expect_false(any(
    c("ratios", "trace_ratios", "cycle_ratios", "scan_ratios") %in% names(res)
  ))
  tr <- res$traces

  # `ratio_name` and `ratio` columns are added right after the intensity column
  expect_true(all(c("ratio_name", "ratio") %in% names(tr)))
  int_pos <- which(names(tr) == "intensity.mV")
  expect_equal(names(tr)[int_pos + 1:2], c("ratio_name", "ratio"))

  # all original rows are kept (base masses included)
  expect_equal(nrow(tr), nrow(agg_traces()$traces))

  # base masses (lowest: 28 for N2, 44 for CO2) carry NA in both ratio columns
  base_rows <- (tr$species == "N2" & tr$mass == "28") |
    (tr$species == "CO2" & tr$mass == "44")
  expect_true(all(is.na(tr$ratio[base_rows])))
  expect_true(all(is.na(tr$ratio_name[base_rows])))
  # non-base rows have ratios
  expect_true(all(!is.na(tr$ratio[!base_rows])))
  expect_setequal(
    unique(tr$ratio_name[!base_rows]),
    c("29/28", "30/28", "45/44", "46/44")
  )

  # values: N2 29/28 at t=0 = 40/100 = 0.4; CO2 46/44 at t=1 = 5/500 = 0.01
  expect_equal(
    tr$ratio[tr$species == "N2" & tr$mass == "29" & tr$time.s == 0],
    0.4
  )
  expect_equal(
    tr$ratio[tr$species == "CO2" & tr$mass == "46" & tr$time.s == 1],
    0.01
  )
})

test_that("ir_calculate_ratios() default offsets are scaled to the data unit", {
  # default num_add.V = 100 V; data is mV -> 100 * 1000 = 100000 mV added to both
  res <- ir_calculate_ratios(agg_traces()) |> suppressMessages()
  tr <- res$traces
  # N2 29/28 at t=0: (40 + 100000) / (100 + 100000)
  expect_equal(
    tr$ratio[tr$species == "N2" & tr$mass == "29" & tr$time.s == 0],
    (40 + 100000) / (100 + 100000)
  )
  # custom, asymmetric offsets: 1 V -> 1000 mV (num), 2 V -> 2000 mV (denom)
  res2 <- ir_calculate_ratios(agg_traces(), num_add.V = 1, denom_add.V = 2) |>
    suppressMessages()
  expect_equal(
    res2$traces$ratio[
      res2$traces$species == "N2" &
        res2$traces$mass == "29" &
        res2$traces$time.s == 0
    ],
    (40 + 1000) / (100 + 2000)
  )
  # the .nA/.cps offsets are ignored for voltage data
  res3 <- ir_calculate_ratios(agg_traces(), num_add.V = 0, num_add.nA = 999) |>
    suppressMessages()
  expect_equal(
    res3$traces$ratio[
      res3$traces$species == "N2" &
        res3$traces$mass == "29" &
        res3$traces$time.s == 0
    ],
    0.4
  )
})

test_that("ir_calculate_ratios() offset family + scaling depends on the unit", {
  agg_current <- function(unit, vals) {
    structure(
      list(
        metadata = tibble(uidx = 1L, analysis = 1L, file_name = "a"),
        traces = tibble(
          uidx = 1L,
          analysis = 1L,
          species = "Ar",
          mass = c("36", "40"),
          time.s = 0,
          !!paste0("intensity.", unit) := vals
        )
      ),
      class = "ir_aggregated_data"
    )
  }

  # nA data, default num_add.nA = 0 -> plain ratio 40/36 = 100/10 = 10
  r_nA <- ir_calculate_ratios(agg_current("nA", c(10, 100))) |>
    suppressMessages()
  expect_equal(r_nA$traces$ratio[r_nA$traces$mass == "40"], 10)
  # nA data, num_add.nA = 5 (scale 1 for nA): (100+5)/(10+5)
  r_nA2 <- ir_calculate_ratios(agg_current("nA", c(10, 100)), num_add.nA = 5) |>
    suppressMessages()
  expect_equal(
    r_nA2$traces$ratio[r_nA2$traces$mass == "40"],
    (100 + 5) / (10 + 5)
  )
  # pA data, num_add.nA = 1 nA -> 1000 pA added: (100+1000)/(10+1000)
  r_pA <- ir_calculate_ratios(agg_current("pA", c(10, 100)), num_add.nA = 1) |>
    suppressMessages()
  expect_equal(
    r_pA$traces$ratio[r_pA$traces$mass == "40"],
    (100 + 1000) / (10 + 1000)
  )
  # cps data uses num_add.cps (default 0) -> plain ratio
  r_cps <- ir_calculate_ratios(agg_current("cps", c(10, 100))) |>
    suppressMessages()
  expect_equal(r_cps$traces$ratio[r_cps$traces$mass == "40"], 10)
})

test_that("ir_calculate_ratios() normalize_ratios accepts a function (or NULL)", {
  # NULL (default) does not normalize
  raw <- ir_calculate_ratios(agg_traces(), num_add.V = 0) |> suppressMessages()
  expect_equal(
    raw$traces$ratio[
      raw$traces$species == "N2" &
        raw$traces$mass == "29" &
        raw$traces$time.s == 0
    ],
    0.4
  )
  # mean: N2 29/28 raw 0.4 (t0), 0.5 (t1), mean 0.45 -> 0.4/0.45
  m <- ir_calculate_ratios(
    agg_traces(),
    num_add.V = 0,
    normalize_ratios = mean
  ) |>
    suppressMessages()
  expect_equal(
    m$traces$ratio[
      m$traces$species == "N2" & m$traces$mass == "29" & m$traces$time.s == 0
    ],
    0.4 / 0.45
  )
  grp_29_28 <- function(res) {
    tr <- res$traces
    tr$ratio[!is.na(tr$ratio_name) & tr$ratio_name == "29/28"]
  }
  # max: the largest ratio in each group becomes 1
  mx <- ir_calculate_ratios(
    agg_traces(),
    num_add.V = 0,
    normalize_ratios = max
  ) |>
    suppressMessages()
  expect_equal(max(grp_29_28(mx)), 1)
  # min normalizes the smallest to 1
  mn <- ir_calculate_ratios(
    agg_traces(),
    num_add.V = 0,
    normalize_ratios = min
  ) |>
    suppressMessages()
  expect_equal(min(grp_29_28(mn)), 1)
  # a non-function, non-NULL value errors
  expect_error(
    ir_calculate_ratios(agg_traces(), normalize_ratios = TRUE),
    "NULL or a function"
  )
})

test_that("ir_calculate_ratios() is idempotent (overwrites prior columns)", {
  once <- ir_calculate_ratios(agg_traces()) |> suppressMessages()
  twice <- ir_calculate_ratios(once) |> suppressMessages()
  # columns are not duplicated and values are identical
  expect_identical(names(twice$traces), names(once$traces))
  expect_identical(twice$traces$ratio, once$traces$ratio)
  expect_identical(twice$traces$ratio_name, once$traces$ratio_name)
  expect_equal(sum(names(twice$traces) == "ratio"), 1L)
})

test_that("ir_calculate_ratios() honors per-species base mass overrides", {
  res <- ir_calculate_ratios(agg_traces(), N2 = 29, num_add.V = 0) |>
    suppressMessages()
  tr <- res$traces

  # N2 now uses 29 as base (NA there); CO2 keeps its default (44)
  expect_true(all(is.na(tr$ratio[tr$species == "N2" & tr$mass == "29"])))
  expect_setequal(
    unique(tr$ratio_name[tr$species == "N2" & !is.na(tr$ratio_name)]),
    c("28/29", "30/29")
  )
  expect_setequal(
    unique(tr$ratio_name[tr$species == "CO2" & !is.na(tr$ratio_name)]),
    c("45/44", "46/44")
  )
  # 28/29 at t=0 = 100/40 = 2.5
  expect_equal(
    tr$ratio[tr$species == "N2" & tr$mass == "28" & tr$time.s == 0],
    2.5
  )
})

test_that("ir_calculate_ratios() warns about unusable base mass specs", {
  # base mass not present for the species -> warn, no ratios for it (all NA)
  expect_warning(
    res <- ir_calculate_ratios(agg_traces(), N2 = 99),
    "not found in the data"
  ) |>
    suppressMessages()
  expect_true(all(is.na(res$traces$ratio[res$traces$species == "N2"])))
  # CO2 (default base) still gets ratios
  expect_true(any(!is.na(res$traces$ratio[res$traces$species == "CO2"])))

  # species not present at all -> warn
  expect_warning(
    ir_calculate_ratios(agg_traces(), Ar = 40) |> suppressMessages(),
    "species not present"
  )
})

test_that("ir_calculate_ratios() handles cycles per type", {
  # dual inlet style data: same cycle number for standard and sample, two masses
  agg <- structure(
    list(
      metadata = tibble(uidx = 1L, analysis = 1L, file_name = "a"),
      cycles = tibble(
        uidx = 1L,
        analysis = 1L,
        species = "CO2",
        cycle = rep(1L, 4),
        type = rep(c("standard", "sample"), each = 2),
        mass = rep(c("44", "45"), 2),
        intensity.mV = c(1000, 100, 1000, 200) # std 45/44=0.1, sample 45/44=0.2
      )
    ),
    class = "ir_aggregated_data"
  )
  # cycles ALWAYS use plain ratios; the additive offsets are ignored for them, so
  # passing a (large) num_add.V must not change the dual inlet ratios
  cy <- (ir_calculate_ratios(agg, num_add.V = 100) |>
    suppressMessages())$cycles

  expect_equal(nrow(cy), 4L) # all rows kept
  # the base mass (44) is NA in both types; 45 gets the plain per-type ratio
  expect_true(all(is.na(cy$ratio[cy$mass == "44"])))
  expect_equal(cy$ratio[cy$type == "standard" & cy$mass == "45"], 0.1)
  expect_equal(cy$ratio[cy$type == "sample" & cy$mass == "45"], 0.2)
})

test_that("ir_calculate_ratios() ignores additive offsets for cycles only", {
  # a dataset with both traces and cycles in mV; a non-zero num_add.V must offset
  # the traces ratio but leave the cycles ratio plain
  agg <- structure(
    list(
      metadata = tibble(uidx = 1L, analysis = 1L, file_name = "a"),
      traces = tibble(
        uidx = 1L,
        analysis = 1L,
        species = "N2",
        mass = c("28", "29"),
        time.s = 0,
        intensity.mV = c(100, 40)
      ),
      cycles = tibble(
        uidx = 1L,
        analysis = 1L,
        species = "CO2",
        cycle = 1L,
        type = "sample",
        mass = c("44", "45"),
        intensity.mV = c(1000, 100)
      )
    ),
    class = "ir_aggregated_data"
  )
  res <- ir_calculate_ratios(agg, num_add.V = 100) |> suppressMessages()
  # traces: (40 + 100000) / (100 + 100000)  (offset applied)
  expect_equal(
    res$traces$ratio[res$traces$mass == "29"],
    (40 + 100000) / (100 + 100000)
  )
  # cycles: plain 100 / 1000 = 0.1  (offset ignored)
  expect_equal(res$cycles$ratio[res$cycles$mass == "45"], 0.1)
})

test_that("ir_calculate_ratios() adds columns to each present series", {
  agg <- structure(
    list(
      metadata = tibble(uidx = 1:2, analysis = 1L, file_name = c("a", "b")),
      traces = tibble(
        uidx = 1L,
        analysis = 1L,
        species = "N2",
        mass = c("28", "29"),
        time.s = 0,
        intensity.mV = c(100, 50)
      ),
      scans = tibble(
        uidx = 2L,
        analysis = 1L,
        species = "Ar",
        mass = c("40", "36"),
        x = 0,
        intensity.mV = c(1000, 10)
      ),
      problems = empty_cnds_tibble()
    ),
    class = "ir_aggregated_data"
  )
  r <- ir_calculate_ratios(agg, num_add.V = 0) |> suppressMessages()

  # both series gain the ratio columns; no new datasets are created
  expect_true(all(c("ratio_name", "ratio") %in% names(r$traces)))
  expect_true(all(c("ratio_name", "ratio") %in% names(r$scans)))
  expect_false(any(
    c("trace_ratios", "scan_ratios", "ratios") %in% names(r)
  ))
  # traces: base 28 (lower than 36 for Ar scans is independent); 29/28 = 0.5
  expect_equal(r$traces$ratio[r$traces$mass == "29"], 0.5)
  # scans: base is the lowest (36); 40/36
  expect_equal(r$scans$ratio_name[r$scans$mass == "40"], "40/36")
})

test_that("ir_calculate_ratios() works end-to-end on the bundled examples", {
  agg <- ir_examples_folder() |>
    ir_find_isofiles() |>
    ir_read_isofiles(show_progress = FALSE, show_problems = FALSE) |>
    suppressMessages() |>
    ir_aggregate_isofiles("mV") |>
    suppressMessages()

  # num_add.V = 0 -> plain intensity ratio, to match the manual check below
  res <- ir_calculate_ratios(agg, num_add.V = 0) |>
    suppressMessages()
  # ratio columns are added to all three present series
  for (ds in c("traces", "cycles", "scans")) {
    expect_true(all(c("ratio_name", "ratio") %in% names(res[[ds]])))
    expect_equal(nrow(res[[ds]]), nrow(agg[[ds]])) # rows preserved
  }

  # spot-check one continuous-flow ratio against the raw traces
  tr <- res$traces
  grp <- tr[tr$species == "N2", ]
  grp <- grp[grp$uidx == grp$uidx[1] & grp$time.s == grp$time.s[1], ]
  manual <- grp$intensity.mV[grp$mass == "29"] /
    grp$intensity.mV[grp$mass == "28"]
  expect_equal(grp$ratio[grp$mass == "29"], manual)
  expect_true(is.na(grp$ratio[grp$mass == "28"])) # base mass
})
