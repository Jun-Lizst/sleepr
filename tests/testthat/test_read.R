context("Reading MDFs")

url <- "https://physionet.org/pn4/sleep-edfx/ST7132J0-PSG.edf"
file <- "data/ST7132J0-PSG.edf"
mdfPath <- "data/ST7132J0-PSG"

if(!file.exists(file)){
  download.file(url,
                destfile = file)
}

sleepr::write.mdf(edfPath = file,
                mdfPath = mdfPath)

mdf <- sleepr::read.mdf(mdfPath = mdfPath)

test_that("Reading all channels", {
  expect_equal(length(mdf[["channels"]]), 5)
})

mdf <- sleepr::read.mdf(mdfPath = mdfPath, channels = c("EEG Fpz-Cz","EOG horizontal"))

test_that("Reading channels subset", {
  expect_equal(length(mdf[["channels"]]), 2)
})

unlink(mdfPath,recursive = TRUE)
