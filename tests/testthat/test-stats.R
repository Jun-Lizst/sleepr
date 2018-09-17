context("Computing stats")

test_that("Computing statistics from one record", {
  write_mdf(edfPath = "data/sample.edf",
            mdfPath = "data/sample",
            events = read_events_noxturnal("data/noxturnal_events_example_unicode.csv"))
  stats <- compute_all_stats(c("data/sample"))
  expect_equal(nrow(stats), 1)
  unlink("data/sample", recursive = TRUE)
})

test_that("REM duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(rem_duration(events), 124.5)
})

test_that("N1 duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(n1_duration(events), 33)
})