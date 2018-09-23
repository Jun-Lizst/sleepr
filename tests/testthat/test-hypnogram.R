context("Creating hypnograms")

test_that("Plotting a hypnogram", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  hypnogram <- hypnogram(events)
  hypnogram <- plot_hypnogram(events)
  expect_equal(class(hypnogram)[1], "gg")
  expect_equal(class(hypnogram)[2], "ggplot")
})

test_that("Plotting a hypnogram without REM", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  events <- events[events$event != "REM",]
  hypnogram <- hypnogram(events)
  hypnogram <- plot_hypnogram(events)
  expect_equal(class(hypnogram)[1], "gg")
  expect_equal(class(hypnogram)[2], "ggplot")
})
