# Tests for the IonOS/LyticOS (.iarc/.larc) reader. These use small hand-written
# JSON fixtures that mimic the isoextract output structure, so they exercise the
# parsing logic without needing isoextract or any real data files.

liarc_fixture <- function(json, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".json", .local_envir = env)
  writeLines(json, f)
  f
}

test_that("read_liarc_tasks() orders tasks by the integer Id", {
  # tasks appear as ids 3, 1, 2 -> should come back ordered 1, 2, 3
  f <- liarc_fixture(
    '{"tasks":[{"id":3,"name":"C"},{"id":1,"name":"A"},{"id":2,"name":"B"}]}'
  )
  tasks <- read_liarc_tasks(f)
  expect_equal(tasks$id, c(1L, 2L, 3L))
  expect_equal(tasks$name, c("A", "B", "C"))
})

test_that("read_liarc_tasks() keeps the original order when Id is absent", {
  f <- liarc_fixture('{"tasks":[{"name":"X"},{"name":"Y"},{"name":"Z"}]}')
  tasks <- read_liarc_tasks(f)
  expect_equal(tasks$name, c("X", "Y", "Z"))
})

test_that("read_liarc_metadata() numbers analysis in Id order", {
  # tasks given as ids 3, 1, 2; metadata should be ordered by Id with
  # analysis = 1..n following that order
  f <- liarc_fixture(paste0(
    '{"created_date":"2020-01-01T00:00:00+00:00",',
    '"processing_lists":[{"guid":"g1","name":"Seq1"}],',
    '"methods":[{"id":10,"name":"M10"}],',
    '"tasks":[',
    '{"id":3,"name":"C","method_id":10,"acquisition_start":"2020-01-01T00:03:00+00:00","acquisition_end":"2020-01-01T00:04:00+00:00"},',
    '{"id":1,"name":"A","method_id":10,"acquisition_start":"2020-01-01T00:01:00+00:00","acquisition_end":"2020-01-01T00:02:00+00:00"},',
    '{"id":2,"name":"B","method_id":10,"acquisition_start":"2020-01-01T00:02:00+00:00","acquisition_end":"2020-01-01T00:03:00+00:00"}',
    ']}'
  ))
  md <- read_liarc_metadata(f)
  expect_equal(md$analysis, c(1L, 2L, 3L))
  expect_equal(md$Id, c(1L, 2L, 3L))
  expect_equal(md$Name, c("A", "B", "C"))
})

test_that("read_liarc_traces() numbers analysis in Id order (matching metadata)", {
  # tasks given as ids 2, 1; the id=1 task carries beam1 = 10, id=2 carries 20.
  # After ordering by Id, analysis 1 must map to the id=1 task (intensity 10).
  f <- liarc_fixture(paste0(
    '{"tasks":[',
    '{"id":2,"method_id":10,"datasets":[{"start":"2020-01-01T00:00:00+00:00","end":"2020-01-01T00:01:00+00:00","data":{"scan":[1,2],"beam1":[20,20]}}]},',
    '{"id":1,"method_id":10,"datasets":[{"start":"2020-01-01T00:00:00+00:00","end":"2020-01-01T00:01:00+00:00","data":{"scan":[1,2],"beam1":[10,10]}}]}',
    ']}'
  ))
  tr <- read_liarc_traces(f, global_species = NULL, method_species = NULL) |>
    suppressWarnings()
  by_analysis <- unique(tr[c("analysis", "intensity.A")])
  expect_equal(by_analysis$analysis, c(1L, 2L))
  # analysis 1 == the id=1 task (intensity 10), analysis 2 == the id=2 task (20)
  expect_equal(by_analysis$intensity.A, c(10, 20))
})
