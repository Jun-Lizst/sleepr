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



# Position & activity ----

# test_that("Back position", {
#   
#   # Real scoring
#   events <- read_events_noxturnal("data/noxturnal_events_example_unicode_2.csv")
#   expect_equal(round(tts_pos_back(events)), 123)
#   expect_equal(round(tts_pos_back_pct(events),digits=3),0.363)
#   
#   # Incorrect dataframe
#   expect_warning(tts_pos_back(data.frame()))
#   
#   # Correct small dataframe
#   events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
#   events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
#   events$event = c("N3","N3","back")
#   expect_equal(round(tts_pos_back(events),digits = 3),0.167)
#   expect_equal(round(tts_pos_back_pct(events),digits = 3),0.167)
#   
#   # Empty dataframe
#   expect_equal(tts_pos_back(data.frame(begin = as.POSIXlt(character()),
#                                        end = as.POSIXlt(character()),
#                                        event = character())),0)
#   expect_equal(tts_pos_back_pct(data.frame(begin = as.POSIXlt(character()),
#                                        end = as.POSIXlt(character()),
#                                        event = character())),0)
#   
#   # Only back events
#   events <- data.frame(begin = as.POSIXlt(c(1536967800),origin = "1970-01-01"))
#   events$end <- as.POSIXlt(c(1536967830), origin = "1970-01-01")
#   events$event = c("back")
#   expect_equal(tts_pos_back(events),0)
#   expect_equal(tts_pos_back_pct(events),0)
#   
#   # Back + AWA
#   events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
#   events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
#   events$event = c("AWA","AWA","back")
#   expect_equal(tts_pos_back(events),0)
#   expect_equal(tts_pos_back_pct(events),0)
# })
# 
# test_that("Left position", {
#   events <- read_events_noxturnal("data/noxturnal_events_example_unicode_2.csv")
#   expect_equal(round(tts_pos_left(events)), 142)
#   expect_equal(round(tts_pos_left_pct(events),digits=3),0.419)
#   expect_warning(tts_pos_left(data.frame()))
#   events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
#   events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
#   events$event = c("N3","N3","left")
#   expect_equal(round(tts_pos_left(events),digits = 3),0.167)
#   expect_equal(round(tts_pos_left_pct(events),digits = 3),0.167)
#   
#   # Empty dataframe
#   expect_equal(tts_pos_left(data.frame(begin = as.POSIXlt(character()),
#                                        end = as.POSIXlt(character()),
#                                        event = character())),0)
#   expect_equal(tts_pos_left(data.frame(begin = as.POSIXlt(character()),
#                                            end = as.POSIXlt(character()),
#                                            event = character())),0)
# })
# 
# test_that("Stomach position", {
#   
#   # Real scoring
#   events <- read_events_noxturnal("data/noxturnal_events_example_unicode_2.csv")
#   expect_equal(round(tts_pos_stomach(events)), 0)
#   expect_equal(round(tts_pos_stomach_pct(events),digits=3),0)
#   
#   # Incorrect dataframe
#   expect_warning(tts_pos_stomach(data.frame()))
#   expect_warning(tts_pos_stomach_pct(data.frame()))
#   
#   # Correct small dataframe
#   events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
#   events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
#   events$event = c("N3","N3","stomach")
#   expect_equal(round(tts_pos_stomach(events),digits = 3),0.167)
#   expect_equal(round(tts_pos_stomach_pct(events),digits = 3),0.167)
#   
#   # Empty dataframe
#   expect_equal(tts_pos_stomach(data.frame(begin = as.POSIXlt(character()),
#                                        end = as.POSIXlt(character()),
#                                        event = character())),0)
#   expect_equal(tts_pos_stomach_pct(data.frame(begin = as.POSIXlt(character()),
#                                        end = as.POSIXlt(character()),
#                                        event = character())),0)
# })
# 
# test_that("Non-back position", {
#   
#   # Real scoring
#   events <- read_events_noxturnal("data/noxturnal_events_example_unicode_2.csv")
#   expect_equal(round(tts_pos_nonback(events)), 216)
#   expect_equal(round(tts_pos_nonback_pct(events),digits=3),0.637)
#   
#   # Incorrect dataframe
#   expect_warning(tts_pos_nonback(data.frame()))
#   expect_warning(tts_pos_nonback_pct(data.frame()))
#   
#   # Correct small dataframe
#   events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
#   events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
#   events$event = c("N3","N3","stomach")
#   expect_equal(round(tts_pos_nonback(events),digits = 3),1)
#   expect_equal(round(tts_pos_nonback_pct(events),digits = 3),1)
#   
#   # Empty dataframe
#   expect_equal(tts_pos_nonback(data.frame(begin = as.POSIXlt(character()),
#                                           end = as.POSIXlt(character()),
#                                           event = character())),0)
#   expect_equal(tts_pos_nonback_pct(data.frame(begin = as.POSIXlt(character()),
#                                               end = as.POSIXlt(character()),
#                                               event = character())),0)
# })
# 
# # Snoring ----
# 
# test_that("Snoring count", {
#   
#   # Real scoring
#   
#   # Incorrect dataframe
#   
#   # Correct small dataframe
#   
#   # Empty dataframe
#   expect_equal(snoring_count(data.frame(begin = as.POSIXlt(character()),
#                                           end = as.POSIXlt(character()),
#                                           event = character())),0)
# })
# 
# test_that("Snoring index", {
#   
#   # Real scoring
#   
#   # Incorrect dataframe
#   
#   # Correct small dataframe
#   
#   # Empty dataframe
#   expect_equal(snoring_index(data.frame(begin = as.POSIXlt(character()),
#                                         end = as.POSIXlt(character()),
#                                         event = character())),0)
# })

# test_that("Snoring duration", {
#   
#   # Real scoring
#   
#   # Incorrect dataframe
#   
#   # Correct small dataframe
#   
#   # Empty dataframe
#   expect_equal(snoring_duration(data.frame(begin = as.POSIXlt(character()),
#                                         end = as.POSIXlt(character()),
#                                         event = character())),0)
# })

# test_that("Snoring duration ratio", {
#   
#   # Real scoring
#   
#   # Incorrect dataframe
#   
#   # Correct small dataframe
#   
#   # Empty dataframe
#   expect_equal(snoring_duration_pct(data.frame(begin = as.POSIXlt(character()),
#                                            end = as.POSIXlt(character()),
#                                            event = character())),0)
# })



test_that("Cycles stats", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_3.csv")
  cycles_stats(events)
})

test_that("MA stats", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  ma_stats(events)
})
