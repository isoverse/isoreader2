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
