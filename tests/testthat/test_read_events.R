context("Reading events")

url <- "http://sleeptight.isr.uc.pt/ISRUC_Sleep/ISRUC_Sleep/subgroupI/1.rar"
file <- "data/1.rar"

if(!file.exists(paste0("data/1/1.rar"))){
  download.file(url,
                destfile = file)
  system(paste0("unrar x ",file, " data/"))
  file.copy(file,to = "data/1/")
  file.remove(file)
}

test_that("Reading ISRUC events", {
  events <- sleepr::read.events.isruc(dir="data/1/", scoringNum=1)
  expect_equal(nrow(events), 880)
})

test_that("Reading Noxturnal events", {
  events <- sleepr::read.events.noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(nrow(events), 1411)
})
