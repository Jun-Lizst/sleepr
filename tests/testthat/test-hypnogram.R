context("Creating hypnograms")

test_that("Noxturnal hypnogram", {
  events <- read.events.noxturnal("data/noxturnal_events_example_unicode.csv")
  hypnogram <- hypnogram(events)
  hypnogram <- plot_hypnogram(events)
  expect_equal(class(hypnogram)[1], "gg")
  expect_equal(class(hypnogram)[2], "ggplot")
})

test_that("ISRUC hypnogram", {
  events <- read.events.isruc("data/1/",scoringNum = 1)
  hypnogram <- hypnogram(events)
  phypnogram <- plot_hypnogram(events)
  expect_equal(class(phypnogram)[1], "gg")
  expect_equal(class(phypnogram)[2], "ggplot")
})
