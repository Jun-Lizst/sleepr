context("Computing stats")

# Download files
if(!file.exists(paste0("data/1/1.rar"))){
  download.file("http://sleeptight.isr.uc.pt/ISRUC_Sleep/ISRUC_Sleep/subgroupI/1.rar",
                destfile =  "data/1.rar")
  system(paste0("unrar x ", "data/1.rar", " data/"))
  file.copy("data/1.rar",to = "data/1/")
  file.remove("data/1.rar")
}

sleepr::write.mdf(edfPath = "data/1/1.rec",
                  mdfPath = "data/1/1",
                  events = sleepr::read.events.isruc(dir="data/1/", scoringNum=1))

sleepr::write.mdf(edfPath = "data/ST7132J0-PSG.edf",
                  mdfPath = "data/ST7132J0",
                  events = sleepr::read_events_sleepedfx("data/ST7132JR-Hypnogram.edf"))

test_that("Computing REM minutes", {
  expect_equal(sleepr::get_rem_minutes(sleepr::read.mdf("data/1/1",c())[["events"]]), 59)
  expect_equal(sleepr::get_rem_minutes(sleepr::read.mdf("data/ST7132J0",c())[["events"]]), 108)
})

test_that("Computing all stats", {
  stats <- sleepr::compute_all_stats(c("data/1/1/",
                                       "data/ST7132J0-PSG/"))
  expect_equal(nrow(stats), 2)
})

# Cleanup
unlink("data/1/1",recursive = TRUE)
unlink("data/ST7132J0",recursive = TRUE)
