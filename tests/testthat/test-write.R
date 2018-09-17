context("Writing records")

test_that("Writing all channels and overwriting with events", {
  
  # First write
  write_mdf(edfPath = "data/sample.edf",
            mdfPath = "data/sample")
  expect_equal(length(list.dirs("data/sample")), 91)
  
  # Overwrite
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  write_mdf(edfPath = "data/sample.edf",
            mdfPath = "data/sample",
            channels = c("Activity","Airflow"),
            events = read_events_noxturnal("data/noxturnal_events_example_unicode.csv"))
  expect_equal(length(list.dirs("data/sample")), 3)
  
  events.write <- jsonlite::read_json("data/sample/events.json",simplifyVector = TRUE)
  expect_equal(length(events.write), length(events))
  expect_equal(nrow(events.write), nrow(events))
  unlink("data/sample",recursive = TRUE)
})

test_that("Do not write channels", {
  sleepr::write_mdf(edfPath = "data/sample.edf",
                    mdfPath = "data/sample",
                    channels = c())
  expect_equal(length(list.dirs("data/sample")), 1)
  unlink("data/sample",recursive = TRUE)
})

# test_that("Invalid EDF path", {
#   expect_error(
#     write_mdf(edfPath = "An invalid path",
#               mdfPath = "",
#               channels = c()))
# })


