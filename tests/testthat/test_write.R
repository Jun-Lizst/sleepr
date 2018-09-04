context("Writing MDFs")

url <- "https://physionet.org/pn4/sleep-edfx/ST7132J0-PSG.edf"
file <- "data/ST7132J0-PSG.edf"
mdfPath <- "data/ST7132J0-PSG"

if(!file.exists(file)){
  download.file(url,
                destfile = file)
}

test_that("Writing all channels", {
  sleepr::write.mdf(edfPath = file,
                  mdfPath = mdfPath)
  expect_equal(length(list.dirs(mdfPath)), 6)
  unlink(mdfPath,recursive = TRUE)
})

test_that("Writing selected channels", {
  sleepr::write.mdf(edfPath = file,
                  mdfPath = mdfPath,
                  channels = c("EEG Fpz-Cz","EOG horizontal"))
  expect_equal(length(list.dirs(mdfPath)), 3)
  unlink(mdfPath,recursive = TRUE)
})




