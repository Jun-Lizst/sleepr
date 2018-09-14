context("Writing records")

file <- "data/ST7132J0-PSG.edf"
mdfPath <- "data/ST7132J0-PSG"

test_that("Writing all channels and overwriting with events", {
  
  # First write
  write_mdf(edfPath = file,
            mdfPath = mdfPath)
  expect_equal(length(list.dirs(mdfPath)), 6)
  
  # Overwrite
  events <- sleepr::read.events.isruc(dir="data/1/", scoringNum=1)
  write_mdf(edfPath = file,
            mdfPath = mdfPath,
            channels = c("EEG Fpz-Cz","EOG horizontal"),
            events = read.events.isruc(dir="data/1/", scoringNum=1))
  expect_equal(length(list.dirs(mdfPath)), 3)
  
  events.write <- jsonlite::read_json("data/ST7132J0-PSG/events.json",simplifyVector = TRUE)
  expect_equal(length(events.write), length(events))
  expect_equal(nrow(events.write), nrow(events))
  unlink(mdfPath,recursive = TRUE)
})

test_that("Writing from multiple EDFs", {
  sleepr::write_mdf(edfPath = c(file,"data/1/1.rec"),
                    mdfPath = mdfPath)
  expect_equal(length(list.dirs(mdfPath)), 25)
  unlink(mdfPath,recursive = TRUE)
})

test_that("Do not write channels", {
  sleepr::write_mdf(edfPath = file,
                    mdfPath = mdfPath,
                    channels = c())
  expect_equal(length(list.dirs(mdfPath)), 1)
  unlink(mdfPath,recursive = TRUE)
})

# test_that("Invalid EDF path", {
#   expect_error(
#     write_mdf(edfPath = "An invalid path",
#               mdfPath = "",
#               channels = c()))
# })


