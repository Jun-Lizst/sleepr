context("Computing statistics")

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
  expect_warning(rem_duration(data.frame()))
})

test_that("N1 duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(n1_duration(events), 33)
  expect_warning(n1_duration(data.frame()))
})

test_that("N2 duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(n2_duration(events), 233)
  expect_warning(n2_duration(data.frame()))
})

test_that("N3 duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(n3_duration(events), 182)
  expect_warning(n3_duration(data.frame()))
})

test_that("AWA duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(awa_duration(events), 106.5)
  expect_warning(awa_duration(data.frame()))
})

test_that("Time To Sleep", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(tts(events), 572.5)
  expect_warning(tts(data.frame()))
})

test_that("REM TTS", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(round(rem_tts(events),digits = 3), 0.217)
  expect_warning(rem_tts(data.frame()))
})

test_that("Sleep latency", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(sleep_latency(events), 54.5)
})