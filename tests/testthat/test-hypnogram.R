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

test_that("Splitting signals", {
  startTime <- as.POSIXlt(edfReader::readEdfHeader("data/sample.edf")$startTime, origin = "1970-01-01")
  events <- data.frame(begin = c(startTime,startTime+30,startTime+60),
                       end = c(startTime+30,startTime+60,startTime+90),
                       event = c("N2","N2","REM"))
  events$event <- as.character(events$event)
  write_mdf(edfPath = "data/sample.edf",
            mdfPath = "data/sample",
            events = events)
  record <- read_mdf(mdfPath = "data/sample",channels = c("C3-M2"))
  signal_splitted <- sleepr::split_signal(signal = record[["channels"]][["C3-M2"]][["signal"]],
                                          hypnogram = record[["events"]],
                                          sRate = record[["channels"]][["C3-M2"]][["metadata"]][["sRate"]])
  expect_equal(length(signal_splitted[[2]]), 6000)
  unlink("data/sample",recursive = TRUE)

})