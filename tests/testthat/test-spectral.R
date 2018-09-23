context("Spectral functions")


test_that("Compute spectral power bands", {
  startTime <- as.POSIXlt(edfReader::readEdfHeader("data/sample.edf")$startTime, origin = "1970-01-01")
  events <- data.frame(begin = c(startTime,startTime+30,startTime+60),
                  end = c(startTime+30,startTime+60,startTime+90),
                  event = c("N2","N2","N3"))
  events$event <- as.character(events$event)
  write_mdf(edfPath = "data/sample.edf",
            mdfPath = "data/sample",
            events = events)
  bands_powers <- hypnogram_band_powers(
    record = read_mdf(mdfPath = "data/sample",
                      channels = c("C3-M2"),
                      metadata = FALSE),
    channel = "C3-M2",bands = list(delta = c(0.5,3.5),
                                   theta = c(3.5,8),
                                   alpha = c(8,12),
                                   beta = c(12,30),
                                   gamma1 = c(30,40)))
  expect_equal(nrow(bands_powers), 3)
  expect_equal(length(colnames(bands_powers)), 7)
  unlink("data/sample",recursive = TRUE)
})


