test_that("metadata functions input checks", {
  for (fn in list(ir_filter_metadata, ir_mutate_metadata)) {
    fn() |> expect_error("must be a set of aggregated isofiles.*collection")
    fn(42) |> expect_error("must be a set of aggregated isofiles.*collection")
  }
  ir_join_metadata(42, tibble(), by = "x") |>
    expect_error("must be a set of aggregated isofiles.*collection")
})

# constructed aggregated dataset and isofiles object for the tests below
make_agg <- function() {
  structure(
    list(
      metadata = tibble(
        uidx = 1:2,
        analysis = c("A1", "A2"),
        file_name = c("a", "b"),
        grp = c("x", "y")
      ),
      traces = tibble(
        uidx = c(1L, 2L),
        analysis = c("A1", "A2"),
        v = c(10, 20)
      ),
      resistors = tibble(uidx = c(1L, 2L), Ohm = c(3e8, 1e11)),
      problems = tibble(uidx = integer(0), message = character(0))
    ),
    class = "ir_aggregated_data"
  )
}

# a minimal ir_isofiles object: one row per file, nested datasets per row,
# linked by `analysis` (no uidx), mirroring ir_read_isofiles() output
make_isofiles <- function() {
  structure(
    tibble(
      file_path = c("a.dxf", "b.dxf"),
      metadata = list(
        tibble(file_name = "a", analysis = "A1", grp = "x"),
        tibble(file_name = "b", analysis = "A2", grp = "y")
      ),
      traces = list(
        tibble(analysis = "A1", v = c(10, 11)),
        tibble(analysis = "A2", v = 20)
      ),
      problems = list(
        tibble(message = character(0)),
        tibble(message = character(0))
      )
    ),
    class = c("ir_isofiles", "tbl_df", "tbl", "data.frame")
  )
}

test_that("ir_filter_metadata() on ir_aggregated_data cascades once", {
  out <- make_agg() |> ir_filter_metadata(grp == "x") |> suppressMessages()
  expect_s3_class(out, "ir_aggregated_data")
  # filtered once on the combined metadata, then cascaded to other datasets
  expect_equal(nrow(out$metadata), 1L)
  expect_equal(out$metadata$file_name, "a")
  expect_equal(nrow(out$traces), 1L) # cascaded by uidx + analysis
  expect_equal(nrow(out$resistors), 1L) # cascaded by uidx
})

test_that("ir_mutate_metadata() / ir_join_metadata() on ir_aggregated_data", {
  m <- make_agg() |>
    ir_mutate_metadata(doubled = uidx * 2) |>
    suppressMessages()
  expect_s3_class(m, "ir_aggregated_data")
  expect_equal(m$metadata$doubled, c(2, 4))

  j <- make_agg() |>
    ir_join_metadata(tibble(uidx = 1:2, extra = c("p", "q")), by = "uidx") |>
    suppressMessages()
  expect_s3_class(j, "ir_aggregated_data")
  expect_equal(j$metadata$extra, c("p", "q"))

  # duplicating join is an error
  make_agg() |>
    ir_join_metadata(
      tibble(uidx = c(1L, 1L), extra = c("p", "q")),
      by = "uidx"
    ) |>
    expect_error("duplicated rows")
})

test_that("ir_filter_metadata() on ir_isofiles applies per row", {
  iso <- make_isofiles()

  # keep everything -> each row's nested data is preserved, type preserved
  keep <- iso |> ir_filter_metadata(!is.na(analysis)) |> suppressMessages()
  expect_s3_class(keep, "ir_isofiles")
  expect_equal(nrow(keep), 2L) # both files retained
  expect_equal(nrow(keep$metadata[[1]]), 1L)
  expect_equal(nrow(keep$traces[[1]]), 2L)
  expect_equal(nrow(keep$traces[[2]]), 1L)

  # a filter that matches no file removes every entry (all metadata -> 0 rows)
  drop <- iso |> ir_filter_metadata(analysis == "nope") |> suppressMessages()
  expect_s3_class(drop, "ir_isofiles")
  expect_equal(nrow(drop), 0L)

  # a filter that matches only the first file drops the other file's entry
  partial <- iso |> ir_filter_metadata(analysis == "A1") |> suppressMessages()
  expect_s3_class(partial, "ir_isofiles")
  expect_equal(nrow(partial), 1L) # second file removed entirely
  expect_equal(partial$metadata[[1]]$file_name, "a")
  expect_equal(nrow(partial$metadata[[1]]), 1L)
  expect_equal(nrow(partial$traces[[1]]), 2L)
})

test_that("ir_mutate_metadata() on ir_isofiles applies per row", {
  out <- make_isofiles() |>
    ir_mutate_metadata(tag = paste0("file:", file_name)) |>
    suppressMessages()
  expect_s3_class(out, "ir_isofiles")
  expect_equal(nrow(out), 2L)
  expect_equal(out$metadata[[1]]$tag, "file:a")
  expect_equal(out$metadata[[2]]$tag, "file:b")
})

test_that("ir_join_metadata() on ir_isofiles applies per row", {
  iso <- make_isofiles()
  y <- tibble(file_name = c("a", "b"), note = c("hello", "world"))
  out <- iso |> ir_join_metadata(y, by = "file_name") |> suppressMessages()
  expect_s3_class(out, "ir_isofiles")
  expect_equal(out$metadata[[1]]$note, "hello")
  expect_equal(out$metadata[[2]]$note, "world")

  # a duplicating join errors (checked per row)
  ydup <- tibble(file_name = c("a", "a"), note = c("x", "y"))
  iso |>
    ir_join_metadata(ydup, by = "file_name") |>
    expect_error("duplicated rows")
})

# an aggregated dataset covering all three mass datasets: file 1 (uidx 1) has
# traces, file 2 cycles, file 3 scans - i.e. no mass is shared by all of them
make_mass_agg <- function() {
  structure(
    list(
      metadata = tibble(
        uidx = 1:3,
        analysis = 1L,
        file_name = c("cf", "di", "scn")
      ),
      traces = tibble(
        uidx = 1L,
        analysis = 1L,
        mass = c("28", "29", "44", "45"),
        time.s = 1,
        intensity.mV = 1:4
      ),
      cycles = tibble(
        uidx = 2L,
        analysis = 1L,
        mass = c("44", "45", "46"),
        cycle = 1L,
        intensity.mV = 1:3
      ),
      scans = tibble(
        uidx = 3L,
        analysis = 1L,
        mass = c("17", "18"),
        x = 1,
        intensity.mV = 1:2
      ),
      resistors = tibble(uidx = 1:3, mass = c("28", "44", "17"), Ohm = 3e8),
      problems = tibble(uidx = integer(0), message = character(0))
    ),
    class = "ir_aggregated_data"
  )
}

# a mixed ir_isofiles collection: the datasets a file type does not have are NULL
# in that row (exactly how ir_read_isofiles() returns a mixed collection)
make_mass_isofiles <- function() {
  structure(
    tibble(
      file_path = c("a.dxf", "b.caf"),
      metadata = list(
        tibble(file_name = "a", analysis = 1L),
        tibble(file_name = "b", analysis = 1L)
      ),
      traces = list(
        tibble(analysis = 1L, mass = c("28", "44"), intensity.V = 1:2),
        NULL
      ),
      cycles = list(
        NULL,
        tibble(analysis = 1L, mass = c("45", "46"), intensity.V = 1:2)
      ),
      problems = list(tibble(), tibble())
    ),
    class = c("ir_isofiles", "tbl_df", "tbl", "data.frame")
  )
}

test_that("ir_filter_masses() input checks", {
  ir_filter_masses() |>
    expect_error("must be a set of aggregated isofiles.*collection")
  ir_filter_masses(42, 44) |>
    expect_error("must be a set of aggregated isofiles.*collection")
  # the mass selection is required
  make_mass_agg() |> ir_filter_masses() |> expect_error("must be provided")
  # a selection that keeps nothing would empty the object
  make_mass_agg() |> ir_filter_masses(NULL) |> expect_error("selects no masses")
  make_mass_agg() |> ir_filter_masses(c()) |> expect_error("selects no masses")
  # a mass that is not in the data errors and lists what is available
  make_mass_agg() |>
    ir_filter_masses(99) |>
    expect_error("not a valid mass selection")
  # nothing to filter without any mass-carrying dataset
  structure(
    list(metadata = tibble(uidx = 1L, file_name = "x")),
    class = "ir_aggregated_data"
  ) |>
    ir_filter_masses(44) |>
    expect_error("no traces.*cycles.*or scans data")
})

test_that("ir_filter_masses() filters all mass datasets of ir_aggregated_data", {
  out <- make_mass_agg() |>
    ir_filter_masses(c("44", "45")) |>
    suppressMessages()
  expect_s3_class(out, "ir_aggregated_data")
  # every mass dataset is filtered to the selection
  expect_equal(out$traces$mass, c("44", "45"))
  expect_equal(out$cycles$mass, c("44", "45"))
  expect_equal(nrow(out$scans), 0L) # 17/18 not selected
  # the scan record has no data left -> dropped from the metadata ...
  expect_equal(out$metadata$file_name, c("cf", "di"))
  # ... and the removal cascades to resistors (by uidx)
  expect_equal(out$resistors$uidx, 1:2)
  # resistors themselves are NOT filtered by mass (they are instrument config)
  expect_equal(out$resistors$mass, c("28", "44"))
})

test_that("ir_filter_masses() resolves the selection across all datasets", {
  # "28" only exists in traces and "18" only in scans - selecting both must work
  # and simply leaves the cycles record without data
  out <- make_mass_agg() |>
    ir_filter_masses(c("28", "18")) |>
    suppressMessages()
  expect_equal(out$traces$mass, "28")
  expect_equal(out$scans$mass, "18")
  expect_equal(nrow(out$cycles), 0L)
  expect_equal(out$metadata$file_name, c("cf", "scn"))
})

test_that("ir_filter_masses() supports the tidyselect syntax", {
  masses <- function(sel) {
    out <- suppressMessages(ir_filter_masses(make_mass_agg(), !!sel))
    unique(c(out$traces$mass, out$cycles$mass, out$scans$mass))
  }
  expect_equal(
    masses(quote(everything())),
    c("28", "29", "44", "45", "46", "17", "18")
  )
  expect_equal(masses(quote(44:46)), c("44", "45", "46"))
  expect_equal(masses(quote(-c("28", "29"))), c("44", "45", "46", "17", "18"))
  expect_equal(masses(quote(starts_with("4"))), c("44", "45", "46"))
  expect_equal(masses(quote(any_of(c("44", "99")))), "44")
  # a bare vector held in a variable selects by name, not by position
  wanted <- c("29", "45")
  expect_equal(masses(quote(all_of(wanted))), c("29", "45"))
})

test_that("ir_filter_masses() keeps ratios with their numerator mass", {
  agg <- make_mass_agg()
  # ratios sit on the rows of their numerator mass (45/44 on the mass 45 rows)
  agg$traces$ratio_name <- c(NA, "29/28", NA, "45/44")
  agg$traces$ratio <- c(NA, 0.5, NA, 0.25)

  kept <- agg |> ir_filter_masses(c("44", "45")) |> suppressMessages()
  expect_equal(kept$traces$ratio_name, c(NA, "45/44"))

  # dropping mass 45 drops its 45/44 ratio; the columns are then all NA and are
  # removed like any other all-NA column (as in ir_filter_metadata())
  base_only <- agg |> ir_filter_masses("44") |> suppressMessages()
  expect_false(any(c("ratio_name", "ratio") %in% names(base_only$traces)))
})

test_that("ir_filter_masses() works per file on ir_isofiles", {
  out <- make_mass_isofiles() |>
    ir_filter_masses(c("44", "45")) |>
    suppressMessages()
  expect_s3_class(out, "ir_isofiles")
  expect_equal(nrow(out), 2L)
  expect_equal(out$traces[[1]]$mass, "44")
  expect_equal(out$cycles[[2]]$mass, "45")
  # datasets a file type does not have stay NULL (and keep their position)
  expect_null(out$cycles[[1]])
  expect_null(out$traces[[2]])

  # the selection is resolved across the WHOLE collection, so a mass present in
  # only one of the files works and leaves the other one without data -> dropped
  only_a <- make_mass_isofiles() |> ir_filter_masses("28") |> suppressMessages()
  expect_equal(nrow(only_a), 1L)
  expect_equal(only_a$metadata[[1]]$file_name, "a")
  expect_equal(only_a$traces[[1]]$mass, "28")
})

test_that("nested dataset operations reach mixed ir_isofiles collections", {
  # traces/cycles are NULL in the rows of the file type that does not have them;
  # they must still be recognized as nested dataset columns (and hence filtered)
  iso <- make_mass_isofiles()
  expect_setequal(
    isofiles_dataset_cols(iso),
    c("metadata", "traces", "cycles", "problems")
  )
  # so ir_filter_metadata() also cascades into them for a mixed collection
  out <- iso |> ir_filter_metadata(analysis == 99) |> suppressMessages()
  expect_equal(nrow(out), 0L)
})

test_that("ir_filter_for_*() keep only the requested measurement type", {
  iso <- structure(
    tibble(
      file_path = c("a.dxf", "b.did", "c.scn"),
      metadata = list(
        tibble(file_name = "a", analysis = 1L, type = "cf"),
        tibble(file_name = "b", analysis = 1L, type = "di"),
        tibble(file_name = "c", analysis = 1L, type = "scan")
      ),
      problems = list(tibble(), tibble(), tibble())
    ),
    class = c("ir_isofiles", "tbl_df", "tbl", "data.frame")
  )

  cf <- iso |> ir_filter_for_continuous_flow() |> suppressMessages()
  di <- iso |> ir_filter_for_dual_inlet() |> suppressMessages()
  sc <- iso |> ir_filter_for_scans() |> suppressMessages()

  expect_s3_class(cf, "ir_isofiles")
  expect_equal(nrow(cf), 1L)
  expect_equal(cf$metadata[[1]]$type, "cf")
  expect_equal(di$metadata[[1]]$type, "di")
  expect_equal(sc$metadata[[1]]$type, "scan")

  # also work on ir_aggregated_data and cascade to the other datasets
  agg <- structure(
    list(
      metadata = tibble(
        uidx = 1:2,
        analysis = c(1L, 1L),
        file_name = c("a", "b"),
        type = c("cf", "di")
      ),
      traces = tibble(uidx = 1L, analysis = 1L, v = 1)
    ),
    class = "ir_aggregated_data"
  )
  agg_cf <- agg |> ir_filter_for_continuous_flow() |> suppressMessages()
  expect_s3_class(agg_cf, "ir_aggregated_data")
  expect_equal(nrow(agg_cf$metadata), 1L)
  expect_equal(agg_cf$metadata$type, "cf")
  # the dual inlet file's metadata is gone, and the cascade keeps the cf trace
  expect_equal(nrow(agg_cf$traces), 1L)
  agg_di <- agg |> ir_filter_for_dual_inlet() |> suppressMessages()
  expect_equal(nrow(agg_di$traces), 0L)
})

test_that("ir_filter_for_*() tolerate a missing/NA type column", {
  # an ir_isofiles where one file errored on read so its metadata has no `type`
  iso <- structure(
    tibble(
      file_path = c("a.dxf", "b.dxf", "c.scn"),
      metadata = list(
        tibble(file_name = "a", analysis = 1L, type = "cf"),
        tibble(file_name = "b"), # errored -> no type column
        tibble(file_name = "c", analysis = 1L, type = "scan")
      ),
      problems = list(tibble(), tibble(), tibble())
    ),
    class = c("ir_isofiles", "tbl_df", "tbl", "data.frame")
  )
  # the type-less file never matches and is dropped (no error)
  cf <- iso |> ir_filter_for_continuous_flow() |> suppressMessages()
  expect_equal(nrow(cf), 1L)
  expect_equal(cf$metadata[[1]]$file_name, "a")
  expect_equal(nrow(ir_filter_for_dual_inlet(iso) |> suppressMessages()), 0L)

  # aggregated metadata with an NA type (errored file) -> dropped, no error
  agg <- structure(
    list(
      metadata = tibble(
        uidx = 1:3,
        analysis = 1L,
        file_name = c("a", "b", "c"),
        type = c("cf", NA, "scan")
      )
    ),
    class = "ir_aggregated_data"
  )
  acf <- agg |> ir_filter_for_continuous_flow() |> suppressMessages()
  expect_equal(acf$metadata$type, "cf")

  # no type column at all -> everything dropped, still no error
  no_type <- structure(
    list(metadata = tibble(uidx = 1L, file_name = "x")),
    class = "ir_aggregated_data"
  )
  expect_no_error(suppressMessages(ir_filter_for_scans(no_type)))
  expect_equal(
    nrow((ir_filter_for_scans(no_type) |> suppressMessages())$metadata),
    0L
  )
})
