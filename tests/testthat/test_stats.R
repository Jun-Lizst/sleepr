context("Computing stats")

url <- "http://sleeptight.isr.uc.pt/ISRUC_Sleep/ISRUC_Sleep/subgroupI/1.rar"
file <- "data/1.rar"

if(!file.exists(paste0("data/1/1.rar"))){
  download.file(url,
                destfile = file)
  system(paste0("unrar x ",file, " data/"))
  file.copy(file,to = "data/1/")
  file.remove(file)
}

events.nox <- sleepr::read.events.noxturnal("data/noxturnal_events_example_unicode.csv")
events.isruc <- sleepr::read.events.isruc(dir="data/1/", scoringNum=1)

test_that("Computing REM minutes", {
  expect_equal(sleepr::get_rem_minutes(events.nox), 124.5)
  expect_equal(sleepr::get_rem_minutes(events.isruc), 59)
})
