context("Reading MDFs")

file <- "data/ST7132J0-PSG.edf"
mdfPath <- "data/ST7132J0-PSG"
sleepr::write_mdf(edfPath = file,
                mdfPath = mdfPath)

test_that("Reading all channels", {
  mdf <- read_mdf(mdfPath = mdfPath)
  expect_equal(length(mdf[["channels"]]), 5)
})

test_that("Reading channels subset", {
  mdf <- read_mdf(mdfPath = mdfPath, channels = c("EEG Fpz-Cz","EOG horizontal"))
  expect_equal(length(mdf[["channels"]]), 2)
})

test_that("Reading no channels", {
  mdf <- read_mdf(mdfPath = mdfPath, channels = c())
  expect_equal(length(mdf), 1)
})

test_that("Do not read metadata", {
  mdf <- read_mdf(mdfPath = mdfPath, channels = c(), metadata = FALSE)
  expect_equal(mdf[["metadata"]], NULL)
})

unlink(mdfPath,recursive = TRUE)