context("Computing statistics")

test_that("Computing statistics from one record", {
  write_mdf(edfPath = "data/subject1.edf",
            mdfPath = "data/sample",
            events = read_events_noxturnal("data/noxturnal_events_example_unicode.csv"))
  stats <- compute_all_stats("data/sample",bands = list(delta = c(0.5,3.5),
                                                        theta = c(3.5,8),
                                                        alpha = c(8,12),
                                                        beta = c(12,30),
                                                        gamma1 = c(30,40)),
                             normalize = c(0,40),butter = 7)
  expect_equal(nrow(stats), 1)
  #expect_equal(stats$tts_pos_nonback[1], 572.5)
  unlink("data/sample", recursive = TRUE)
})

test_that("Testing events dataframe checking.", {
  
  e <- data.frame(begin = as.POSIXlt(seq(from = 0, to = 30*10, by = 30),origin = "1970-01-01"))
  e$end <- as.POSIXlt(seq(from = 30, to = 30*11, by = 30), origin = "1970-01-01")
  e$event = c("AWA","N1","N2","N3","N3","REM","N2","REM","N2","REM","AWA")
  
  
  expect_error(check_events(e[,c("end","event")]))
  expect_error(check_events(e[,c("begin","event")]))
  expect_error(check_events(e[,c("end","begin")]))
  
  e2 <- e
  e2$begin <- as.character(e2$begin)
  expect_error(check_events(e2))
  
  e2 <- e
  e2$end <- as.character(e2$end)
  expect_error(check_events(e2))
  
  e2 <- e
  e2$event <- as.factor(e2$event)
  expect_error(check_events(e2))
})

test_that("stages stats", {
  
  e <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  
  r <- stages_stats(e)
  
  expect_equal(r[["rem_duration"]],  124.5)
  expect_equal(r[["n1_duration"]],  33)
  expect_equal(r[["n2_duration"]],  233)
  expect_equal(r[["n3_duration"]],  182)
  expect_equal(r[["awa_duration"]],  106.5)
  expect_equal(r[["tts"]],  572.5)
  expect_equal(r[["rem_tts"]],  0.21746725)
  expect_equal(r[["n1_tts"]], 0.05764192)
  expect_equal(r[["n2_tts"]], 0.40698690)
  expect_equal(r[["n3_tts"]], 0.31790393)
  expect_equal(r[["awa_tts"]], 0.18602620)
  expect_equal(r[["tsp"]], 748.5)
  expect_equal(r[["efficiency"]], 0.76486306)
  expect_equal(r[["latency"]], 54.5)
  expect_equal(r[["n1_latency"]], 0)
  expect_equal(r[["n2_latency"]], 4)
  expect_equal(r[["n3_latency"]], 18)
  expect_equal(r[["rem_latency"]], 107)
  expect_equal(r[["waso"]], 121.5)
  
})

test_that("Cycles stats", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_3.csv")
  cycles_stats(events)
})

test_that("MA stats", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  ma_stats(events)
})
