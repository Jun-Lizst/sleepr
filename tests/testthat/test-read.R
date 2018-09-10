context("Reading MDFs")

file <- "data/ST7132J0-PSG.edf"
mdfPath <- "data/ST7132J0-PSG"
sleepr::write.mdf(edfPath = file,
                mdfPath = mdfPath)

test_that("Reading all channels", {
  mdf <- sleepr::read.mdf(mdfPath = mdfPath)
  expect_equal(length(mdf[["channels"]]), 5)
})

test_that("Reading channels subset", {
  mdf <- sleepr::read.mdf(mdfPath = mdfPath, channels = c("EEG Fpz-Cz","EOG horizontal"))
  expect_equal(length(mdf[["channels"]]), 2)
})

test_that("Reading no channels", {
  mdf <- sleepr::read.mdf(mdfPath = mdfPath, channels = c())
  expect_equal(length(mdf), 1)
})

unlink(mdfPath,recursive = TRUE)