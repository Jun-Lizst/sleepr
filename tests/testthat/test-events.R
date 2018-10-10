context("Reading events")

test_that("Reading Noxturnal events", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(nrow(events), 1411)
})

test_that("Reading Sleep EDF Expanded events", {
  events <- sleepr::read_events_sleepedfx("data/ST7132JR-Hypnogram.edf")
  expect_equal(nrow(events), 852)
})

# test_that("Reading ISRUC events", {
#   events <- sleepr::read_events_isruc("data/isruc/",1)
#   expect_equal(nrow(events), 852)
# })
