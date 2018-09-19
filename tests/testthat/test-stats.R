context("Computing statistics")

test_that("Computing statistics from one record", {
  write_mdf(edfPath = "data/sample.edf",
            mdfPath = "data/sample",
            events = read_events_noxturnal("data/noxturnal_events_example_unicode.csv"))
  stats <- compute_all_stats(c("data/sample"))
  expect_equal(nrow(stats), 1)
  unlink("data/sample", recursive = TRUE)
})

# Stages and scoring ----

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

test_that("N3 TTS", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(round(n3_tts(events),digits = 3), 0.318)
  expect_warning(n3_tts(data.frame()))
})

test_that("N2 TTS", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(round(n2_tts(events),digits = 3), 0.407)
  expect_warning(n2_tts(data.frame()))
})

test_that("N1 TTS", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(round(n1_tts(events),digits = 3), 0.058)
  expect_warning(n1_tts(data.frame()))
})

test_that("Total Sleep Period", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(tsp(events), 748.5)
  expect_warning(tsp(data.frame()))
})

test_that("Sleep Efficiency", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(round(sleep_efficiency(events),digits = 3), 0.765)
  expect_warning(sleep_efficiency(data.frame()))
})

test_that("Sleep latency", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(sleep_latency(events), 54.5)
  expect_warning(sleep_latency(data.frame()))
})

test_that("REM Latency", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(rem_latency(events), 107)
  expect_warning(rem_latency(data.frame()))
})

test_that("Wake After Sleep Onset", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(waso(events), 121.5)
  expect_warning(waso(data.frame()))
})

# Position & activity ----

test_that("Back position", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_2.csv")
  expect_equal(round(tts_pos_back(events)), 123)
  expect_equal(round(tts_pos_back_pct(events),digits=3),0.363)
  expect_warning(tts_pos_back(data.frame()))
  events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
  events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
  events$event = c("N3","N3","back")
  expect_equal(round(tts_pos_back(events),digits = 3),0.167)
  expect_equal(round(tts_pos_back_pct(events),digits = 3),0.167)
})

# Respiratory indexes ----

test_that("Apnea and hypnonea count", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_3.csv")
  expect_equal(ah_count(events), 102)
})