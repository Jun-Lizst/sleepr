context("Reading events")

test_that("Reading ISRUC events", {
  events <- sleepr::read.events.isruc(dir="data/1/", scoringNum=1)
  expect_equal(nrow(events), 880)
})

test_that("Reading Noxturnal events", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(nrow(events), 1411)
})

if(!file.exists("data/ST7132JR-Hypnogram.edf")){
  download.file("https://www.physionet.org/pn4/sleep-edfx/ST7132JR-Hypnogram.edf",
                destfile = "data/ST7132JR-Hypnogram.edf")
}

test_that("Reading Sleep EDF Expanded events", {
  events <- sleepr::read_events_sleepedfx("data/ST7132JR-Hypnogram.edf")
  expect_equal(nrow(events), 852)
})


