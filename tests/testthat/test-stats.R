context("Computing stats")

sleepr::write_mdf(edfPath = "data/1/1.rec",
                  mdfPath = "data/1/1",
                  events = sleepr::read.events.isruc(dir="data/1/", scoringNum=1))

sleepr::write_mdf(edfPath = "data/ST7132J0-PSG.edf",
                  mdfPath = "data/ST7132J0",
                  events = sleepr::read_events_sleepedfx("data/ST7132JR-Hypnogram.edf"))

test_that("Computing REM minutes", {
  expect_equal(rem_minutes(read_mdf("data/1/1",c())[["events"]]), 59)
  expect_equal(rem_minutes(read_mdf("data/ST7132J0",c())[["events"]]), 108)
})

test_that("Computing all stats", {
  stats <- compute_all_stats(c("data/1/1/",
                               "data/ST7132J0/"))
  expect_equal(nrow(stats), 2)
})

# Cleanup
unlink("data/1/1",recursive = TRUE)
unlink("data/ST7132J0",recursive = TRUE)
