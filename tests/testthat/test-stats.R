context("Computing statistics")

test_that("Computing statistics from records", {
  write_mdf(edfPath = "data/subject1.edf",
            mdfPath = "data/sample",
            events = read_events_noxturnal("data/noxturnal_events_example_unicode.csv"))
  stats <- compute_all_stats(c("data/sample","data/sample"),bands = list(delta = c(0.5,3.5),
                                                        theta = c(3.5,8),
                                                        alpha = c(8,12),
                                                        beta = c(12,30),
                                                        gamma1 = c(30,40)),
                             normalize = c(0,40),butter = 7)
  expect_equal(nrow(stats), 2)
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

test_that("Stages statistics", {
  
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
  
  # AWA only events
  e <- e[e$event == "AWA",]
  
  r <- stages_stats(e)
  
  expect_equal(r[["rem_duration"]],  0)
  expect_equal(r[["n1_duration"]],  0)
  expect_equal(r[["n2_duration"]],  0)
  expect_equal(r[["n3_duration"]],  0)
  expect_equal(r[["awa_duration"]],  106.5)
  expect_equal(r[["tts"]],  0)
  expect_equal(r[["rem_tts"]],  0)
  expect_equal(r[["n1_tts"]], 0)
  expect_equal(r[["n2_tts"]], 0)
  expect_equal(r[["n3_tts"]], 0)
  expect_equal(r[["awa_tts"]], 0.)
  expect_equal(r[["tsp"]], 748.5)
  expect_equal(r[["efficiency"]], 0)
  expect_true(is.na(r[["latency"]]))
  expect_true(is.na(r[["waso"]]))
})

test_that("Position statistics", {
  
  e <- read_events_noxturnal("data/noxturnal_events_example_unicode_2.csv")
  
  r <- pos_stats(e)
  
  expect_equal(r[["tts_back"]], 123.3333333)
  expect_equal(round(r[["tts_back_pct"]],digits = 3), 0.363)
  expect_equal(r[["tts_left"]], 142.1666667)
  expect_equal(round(r[["tts_left_pct"]],digits = 3), 0.419)
  expect_equal(r[["tts_right"]], 69.0833333)
  expect_equal(round(r[["tts_right_pct"]],digits = 3),  0.203)
  expect_equal(r[["tts_stomach"]],  0)
  expect_true(is.na(r[["tts_stomach_pct"]]))
  expect_equal(r[["tts_nonback"]],  211.25)
  expect_equal(r[["tts_nonback_pct"]],  0.6222386)
  
  e$event[e$event == "back"] <- "stomach"
  
  r <- pos_stats(e)
  
  expect_equal(r[["tts_back"]], 0)
  expect_true(is.na(r[["tts_back_pct"]]))
  expect_equal(r[["tts_left"]], 142.1666667)
  expect_equal(round(r[["tts_left_pct"]],digits = 3), 0.419)
  expect_equal(r[["tts_right"]], 69.0833333)
  expect_equal(round(r[["tts_right_pct"]],digits = 3),  0.203)
  expect_equal(r[["tts_stomach"]],  123.3333333)
  expect_equal(round(r[["tts_stomach_pct"]],digits = 3),  0.363)
  expect_equal(r[["tts_nonback"]],  334.5833333)
  expect_equal(round(r[["tts_nonback_pct"]],digits = 3),  0.986)
})

test_that("Snoring stats", {
  e <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  e$event[e$event == "Micro-éveil"] <- "Train de ronflements"
  r <- snoring_stats(e)
  expect_equal(r[["snoring_count"]], 19)
  expect_equal(round(r[["snoring_idx"]],digits = 3), 1.991)
  expect_equal(round(r[["snoring_duration"]],digits = 3), 2.533)
})


test_that("Cycles stats", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_3.csv")
  cycles_stats(events)
})

test_that("MA stats", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  ma_stats(events)
})
