context("Reading events")

test_that("Reading Noxturnal events", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(nrow(events), 1411)
})

test_that("Reading Sleep EDF Expanded events", {
  events <- sleepr::read_events_sleepedfx("data/ST7132JR-Hypnogram.edf")
  expect_equal(nrow(events), 852)
})

test_that("Reading Dreams subjects events", {
  events <- sleepr::read_events_dreams_subjects(record_id = "subject1", path = "data/")
  expect_equal(nrow(events), 5768)
})

test_that("Reading ISRUC events", {
  e <- sleepr::read_events_isruc(dir = "data/isruc/1/1/", scoringNum = 1)
  expect_equal(nrow(e), 880)
  
  e <- read_all_events_isruc("data/isruc/")
  expect_equal(nrow(e), 880)
})

test_that("Reading ISRUC metadata", {
  m <- read_isruc_metadata("data/isruc/")
  expect_equal(nrow(m), 126)
})
