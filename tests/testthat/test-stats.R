context("Computing statistics")

# Meta function ----

test_that("Computing statistics from one record", {
  write_mdf(edfPath = "data/subject1.edf",
            mdfPath = "data/sample",
            events = read_events_noxturnal("data/noxturnal_events_example_unicode.csv"))
  stats <- compute_all_stats("data/sample",bands = list(delta = c(0.5,3.5),
                                                        theta = c(3.5,8),
                                                        alpha = c(8,12),
                                                        beta = c(12,30),
                                                        gamma1 = c(30,40)),
                             normalize = c(0,40))
  expect_equal(nrow(stats), 1)
  expect_equal(stats$tts_pos_nonback[1], 572.5)
  unlink("data/sample", recursive = TRUE)
})

# Stages and scoring ----

test_that("REM duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(rem_duration(events), 124.5)
  expect_warning(rem_duration(data.frame()))
})

test_that("N1 duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(n1_duration(events), 33)
  expect_warning(n1_duration(data.frame()))
})

test_that("N2 duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(n2_duration(events), 233)
  expect_warning(n2_duration(data.frame()))
})

test_that("N3 duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(n3_duration(events), 182)
  expect_warning(n3_duration(data.frame()))
})

test_that("AWA duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(awa_duration(events), 106.5)
  expect_warning(awa_duration(data.frame()))
})

test_that("Time To Sleep", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(tts(events), 572.5)
  expect_warning(tts(data.frame()))
})

test_that("REM TTS", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(round(rem_tts(events),digits = 3), 0.217)
  expect_warning(rem_tts(data.frame()))
})

test_that("N3 TTS", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(round(n3_tts(events),digits = 3), 0.318)
  expect_warning(n3_tts(data.frame()))
})

test_that("N2 TTS", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(round(n2_tts(events),digits = 3), 0.407)
  expect_warning(n2_tts(data.frame()))
})

test_that("N1 TTS", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(round(n1_tts(events),digits = 3), 0.058)
  expect_warning(n1_tts(data.frame()))
})

test_that("Total Sleep Period", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(tsp(events), 748.5)
  expect_warning(tsp(data.frame()))
})

test_that("Sleep Efficiency", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(round(sleep_efficiency(events),digits = 3), 0.765)
  expect_warning(sleep_efficiency(data.frame()))
})

test_that("Sleep latency", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(sleep_latency(events), 54.5)
  expect_warning(sleep_latency(data.frame()))
})

test_that("REM latency", {
  
  # Real record
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(rem_latency(events), 107)
  
  # Example 1
  events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
  events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
  events$event = c("AWA","REM")
  expect_equal(rem_latency(events), 0)
  
  # Example 2
  events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
  events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
  events$event = c("N1","REM")
  expect_equal(rem_latency(events), 0.5)
  
  # Example 2
  events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
  events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
  events$event = c("REM","REM")
  expect_equal(rem_latency(events), 0)
  
  # Empty dataframe
  expect_warning(rem_latency(data.frame()))
})

test_that("Wake After Sleep Onset", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode.csv")
  expect_equal(waso(events), 121.5)
  expect_warning(waso(data.frame()))
})

# Position & activity ----

test_that("Back position", {
  
  # Real scoring
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_2.csv")
  expect_equal(round(tts_pos_back(events)), 123)
  expect_equal(round(tts_pos_back_pct(events),digits=3),0.363)
  
  # Incorrect dataframe
  expect_warning(tts_pos_back(data.frame()))
  
  # Correct small dataframe
  events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
  events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
  events$event = c("N3","N3","back")
  expect_equal(round(tts_pos_back(events),digits = 3),0.167)
  expect_equal(round(tts_pos_back_pct(events),digits = 3),0.167)
  
  # Empty dataframe
  expect_equal(tts_pos_back(data.frame(begin = as.POSIXlt(character()),
                                       end = as.POSIXlt(character()),
                                       event = character())),0)
  expect_equal(tts_pos_back_pct(data.frame(begin = as.POSIXlt(character()),
                                       end = as.POSIXlt(character()),
                                       event = character())),0)
  
  # Only back events
  events <- data.frame(begin = as.POSIXlt(c(1536967800),origin = "1970-01-01"))
  events$end <- as.POSIXlt(c(1536967830), origin = "1970-01-01")
  events$event = c("back")
  expect_equal(tts_pos_back(events),0)
  expect_equal(tts_pos_back_pct(events),0)
  
  # Back + AWA
  events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
  events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
  events$event = c("AWA","AWA","back")
  expect_equal(tts_pos_back(events),0)
  expect_equal(tts_pos_back_pct(events),0)
})

test_that("Left position", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_2.csv")
  expect_equal(round(tts_pos_left(events)), 142)
  expect_equal(round(tts_pos_left_pct(events),digits=3),0.419)
  expect_warning(tts_pos_left(data.frame()))
  events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
  events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
  events$event = c("N3","N3","left")
  expect_equal(round(tts_pos_left(events),digits = 3),0.167)
  expect_equal(round(tts_pos_left_pct(events),digits = 3),0.167)
  
  # Empty dataframe
  expect_equal(tts_pos_left(data.frame(begin = as.POSIXlt(character()),
                                       end = as.POSIXlt(character()),
                                       event = character())),0)
  expect_equal(tts_pos_left(data.frame(begin = as.POSIXlt(character()),
                                           end = as.POSIXlt(character()),
                                           event = character())),0)
})

test_that("Stomach position", {
  
  # Real scoring
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_2.csv")
  expect_equal(round(tts_pos_stomach(events)), 0)
  expect_equal(round(tts_pos_stomach_pct(events),digits=3),0)
  
  # Incorrect dataframe
  expect_warning(tts_pos_stomach(data.frame()))
  expect_warning(tts_pos_stomach_pct(data.frame()))
  
  # Correct small dataframe
  events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
  events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
  events$event = c("N3","N3","stomach")
  expect_equal(round(tts_pos_stomach(events),digits = 3),0.167)
  expect_equal(round(tts_pos_stomach_pct(events),digits = 3),0.167)
  
  # Empty dataframe
  expect_equal(tts_pos_stomach(data.frame(begin = as.POSIXlt(character()),
                                       end = as.POSIXlt(character()),
                                       event = character())),0)
  expect_equal(tts_pos_stomach_pct(data.frame(begin = as.POSIXlt(character()),
                                       end = as.POSIXlt(character()),
                                       event = character())),0)
})

test_that("Non-back position", {
  
  # Real scoring
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_2.csv")
  expect_equal(round(tts_pos_nonback(events)), 216)
  expect_equal(round(tts_pos_nonback_pct(events),digits=3),0.637)
  
  # Incorrect dataframe
  expect_warning(tts_pos_nonback(data.frame()))
  expect_warning(tts_pos_nonback_pct(data.frame()))
  
  # Correct small dataframe
  events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
  events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
  events$event = c("N3","N3","stomach")
  expect_equal(round(tts_pos_nonback(events),digits = 3),1)
  expect_equal(round(tts_pos_nonback_pct(events),digits = 3),1)
  
  # Empty dataframe
  expect_equal(tts_pos_nonback(data.frame(begin = as.POSIXlt(character()),
                                          end = as.POSIXlt(character()),
                                          event = character())),0)
  expect_equal(tts_pos_nonback_pct(data.frame(begin = as.POSIXlt(character()),
                                              end = as.POSIXlt(character()),
                                              event = character())),0)
})

# Snoring ----

test_that("Snoring count", {
  
  # Real scoring
  
  # Incorrect dataframe
  
  # Correct small dataframe
  
  # Empty dataframe
  expect_equal(snoring_count(data.frame(begin = as.POSIXlt(character()),
                                          end = as.POSIXlt(character()),
                                          event = character())),0)
})

test_that("Snoring index", {
  
  # Real scoring
  
  # Incorrect dataframe
  
  # Correct small dataframe
  
  # Empty dataframe
  expect_equal(snoring_index(data.frame(begin = as.POSIXlt(character()),
                                        end = as.POSIXlt(character()),
                                        event = character())),0)
})

test_that("Snoring duration", {
  
  # Real scoring
  
  # Incorrect dataframe
  
  # Correct small dataframe
  
  # Empty dataframe
  expect_equal(snoring_duration(data.frame(begin = as.POSIXlt(character()),
                                        end = as.POSIXlt(character()),
                                        event = character())),0)
})

test_that("Snoring duration ratio", {
  
  # Real scoring
  
  # Incorrect dataframe
  
  # Correct small dataframe
  
  # Empty dataframe
  expect_equal(snoring_duration_pct(data.frame(begin = as.POSIXlt(character()),
                                           end = as.POSIXlt(character()),
                                           event = character())),0)
})

# Respiratory indexes ----

test_that("Apnea and hypnonea count", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_3.csv")
  expect_equal(ah_count(events), 102)
})

# Cycles ----

test_that("Cycles count", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_3.csv")
  expect_equal(cycles_classic_count(events), 4)
  expect_equal(cycles_begin_count(events),0)
  expect_equal(cycles_rem_count(events),5)
  expect_equal(cycles_end_count(events),1)
})

test_that("Cycles duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_3.csv")
  expect_equal(cycles_classic_duration(events), 344.35)
  expect_equal(cycles_begin_duration(events), 0)
  expect_equal(cycles_rem_duration(events), 139.25)
  expect_equal(round(cycles_end_duration(events)), 61)
})

test_that("Cycles average duration", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_3.csv")
  expect_equal(cycles_classic_avg_duration(events), 86.0875)
  expect_equal(cycles_begin_avg_duration(events), 0)
  expect_equal(cycles_rem_avg_duration(events), 27.85)
  expect_equal(round(cycles_end_avg_duration(events)), 61)
})

test_that("Cycles stats", {
  events <- read_events_noxturnal("data/noxturnal_events_example_unicode_3.csv")
  cycles_stats(events)
})


