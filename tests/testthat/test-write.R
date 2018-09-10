context("Writing MDFs")

file <- "data/ST7132J0-PSG.edf"
mdfPath <- "data/ST7132J0-PSG"

test_that("Writing all channels", {
  sleepr::write_mdf(edfPath = file,
                  mdfPath = mdfPath)
  expect_equal(length(list.dirs(mdfPath)), 6)
  unlink(mdfPath,recursive = TRUE)
})

test_that("Writing and overwriting", {
  sleepr::write_mdf(edfPath = file,
                    mdfPath = mdfPath)
  sleepr::write_mdf(edfPath = file,
                    mdfPath = mdfPath)
  expect_equal(length(list.dirs(mdfPath)), 6)
  unlink(mdfPath,recursive = TRUE)
})

test_that("Writing from multiple EDFs", {
  sleepr::write_mdf(edfPath = c(file,"data/1/1.rec"),
                    mdfPath = mdfPath)
  expect_equal(length(list.dirs(mdfPath)), 25)
  unlink(mdfPath,recursive = TRUE)
})

test_that("Writing selected channels", {
  sleepr::write_mdf(edfPath = file,
                  mdfPath = mdfPath,
                  channels = c("EEG Fpz-Cz","EOG horizontal"))
  expect_equal(length(list.dirs(mdfPath)), 3)
  unlink(mdfPath,recursive = TRUE)
})

test_that("Do not write channels", {
  sleepr::write_mdf(edfPath = file,
                    mdfPath = mdfPath,
                    channels = c())
  expect_equal(length(list.dirs(mdfPath)), 1)
  unlink(mdfPath,recursive = TRUE)
})

test_that("Writing events", {
  events <- sleepr::read.events.isruc(dir="data/1/", scoringNum=1)
  sleepr::write_mdf(edfPath = "data/1/1.rec",
                    mdfPath = "data/1/mdf/",
                    events = sleepr::read.events.isruc(dir="data/1/", scoringNum=1))
  events.write <- jsonlite::read_json("data/1/mdf/events.json",simplifyVector = TRUE)
  expect_equal(length(events.write), length(events))
  expect_equal(nrow(events.write), nrow(events))
  unlink("data/1/mdf/",recursive = TRUE)
})


