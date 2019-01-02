#' Filter x events overlapping y events.
#'
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param x Events labels overlapping y events.
#' @param y Events labels overlapped by x events.
#' @return Dataframe of x events overlapping y events.
get_overlapping_events <- function(e, x, y){
  x <- e[e$event %in% x,]
  y <- e[e$event %in% y,]
  
  if(nrow(x) > 0 & nrow(y) > 0){
    x$dummy <- TRUE
    y$dummy <- TRUE
    z <- merge(x,y,by="dummy")
    z$dummy <- NULL
    
    z <- z[z$begin.x >= z$begin.y & 
             z$begin.x < z$end.y,]
    
    return(z)
  }
  else {
    return(data.frame())
  }
}

#' Get the duration of x labels events overlapping y labels events.
#'
#' @param x label
#' @param y stages
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
get_overlapping_duration <- function(x, y, e){
  es <- e[e$event %in% x,] # [e]vents [s]ubset 
  h <- hypnogram(e)
  td <- 0
  for(i in c(1:nrow(es))){
    ed <- 0
    hes <- h[(h$begin<=es[i,]$begin & h$end>es[i,]$begin)
             | (h$begin>=es[i,]$begin & h$end<=es[i,]$end)
             | (h$begin<=es[i,]$end & h$end>=es[i,]$end),]
    if(nrow(hes) > 0 & nrow(h) > 0){
      for(j in c(1:nrow(hes))){
        if(hes$event[j] %in% y){
          hi <- lubridate::interval(hes$begin[j], hes$end[j])
          esi <- lubridate::interval(es$begin[i], es$end[i])
          ed <- ed + as.numeric(
            lubridate::as.duration(
              lubridate::intersect(
                hi, esi)),"seconds")
        }
      }
    }
    td <- td+ed
  }
  return(td/60)
}

#' check_events
#'
#' @param e events
check_events <- function(e){
  if(!("begin" %in% colnames(e))){
    stop("Events dataframe must contain a 'begin' column.")
  } else if(!("end" %in% colnames(e))){
    stop("Events dataframe must contain a 'end' column.")
  } else  if(!("event" %in% colnames(e))){
    stop("Events dataframe must contain a 'event' column.")
  } else if(!("POSIXt" %in% class(e$begin))){
    stop("'begin' column must be a datetime.")
  } else if(!("POSIXt" %in% class(e$end))){
    stop("'end' column must be a datetime.")
  } else if(!("character" %in% class(e$event))){
    stop("'events' column must be character type.")
  }
}

#' Get stages events related stats in a named vector.
#'
#' \code{stages_stats} computes stages related stats.
#'
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return stages vector
#' @examples
#' e <- data.frame(begin = as.POSIXlt(seq(from = 0, to = 30*10, by = 30),origin = "1970-01-01"))
#' e$end <- as.POSIXlt(seq(from = 30, to = 30*11, by = 30), origin = "1970-01-01")
#' e$event = c("AWA","N1","N2","N3","N3","REM","N2","REM","N2","REM","AWA")
#' stages_stats(e)
#' @export
stages_stats <- function(e){
  
  # Check events dataframe
  check_events(e)
  
  # Stages duration
  r = c("rem_duration" = sum(as.numeric(difftime(e$end[e$event == "REM"],e$begin[e$event == "REM"],units="min"))))
  r = c(r, "n1_duration" = sum(as.numeric(difftime(e$end[e$event == "N1"],e$begin[e$event == "N1"],units="min"))))
  r = c(r, "n2_duration" = sum(as.numeric(difftime(e$end[e$event == "N2"],e$begin[e$event == "N2"],units="min"))))
  r = c(r, "n3_duration" = sum(as.numeric(difftime(e$end[e$event == "N3"],e$begin[e$event == "N3"],units="min"))))
  r = c(r, "awa_duration" = sum(as.numeric(difftime(e$end[e$event == "AWA"],e$begin[e$event == "AWA"],units="min"))))
  
  # Time To Sleep (TTS)
  r = c(r, "tts" = sum(as.numeric(difftime(e$end[e$event %in% c("N1", "N2", "N3", "REM")],e$begin[e$event  %in% c("N1", "N2", "N3", "REM")],units="min"))))
  
  # Time To Sleep (TTS) 
  r = c(r, "rem_tts" = ifelse(r[["tts"]] == 0, 0, r[["rem_duration"]]/r[["tts"]]))
  r = c(r, "n1_tts" = ifelse(r[["tts"]] == 0, 0, r[["n1_duration"]]/r[["tts"]]))
  r = c(r, "n2_tts" = ifelse(r[["tts"]] == 0, 0, r[["n2_duration"]]/r[["tts"]]))
  r = c(r, "n3_tts" = ifelse(r[["tts"]] == 0, 0, r[["n3_duration"]]/r[["tts"]]))
  r = c(r, "awa_tts" = ifelse(r[["tts"]] == 0, 0, r[["awa_duration"]]/r[["tts"]]))
  
  # TSP Total Sleep Period
  r = c(r, "tsp" = as.numeric(difftime(max(e$end), min(e$begin), units="mins")))
  
  # Sleep efficiency
  r = c(r, "efficiency" = ifelse(r[["tsp"]] == 0, 0, r[["tts"]]/r[["tsp"]]))

  # Latencies
  sleep <- e[e$event %in% c("N1","N2","N3","REM"),]
  if(nrow(sleep) > 0){
    r = c(r, "latency" = as.numeric(difftime(min(sleep$begin),min(e$begin),units="mins")))
  } else {
    r = c(r, "latency" = NA)
  }
  
  # Stages latencies
  for(s in c("N1", "N2", "N3", "REM")){
    
    ss <- e[e$event == s,]
    
    if(nrow(ss) > 0){
      start <- min(ss$begin)
      
      r[[paste0(tolower(s),"_latency")]] <- as.numeric(difftime(start, min(e$begin), units="mins")) - r[["latency"]]
    }
  }
  
  # WASO
  r[["waso"]] <- r[["tsp"]] - r[["latency"]] - r[["tts"]]

  r
}


#' Get position g events related stats in a named vector.
#'
#' \code{snoring_stats} computes snoring related stats.
#'
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param ss sleep stages
#' @return Total snoring count.
#' @examples
#' e <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
#' e$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
#' e$event = c("N3","N3","back")
#' pos_stats(e)
#' @export
pos_stats <- function(e, ss = c("N1","N2","N3","REM")){
  
  # Check events dataframe
  check_events(e)
  
  tts <- sum(as.numeric(difftime(e$end[e$event %in% ss],e$begin[e$event %in% ss],units="mins")))
  
  # Back
  stats <- c("tts_back" = get_overlapping_duration("back", ss ,e))
  
  if(stats[["tts_back"]] == 0 | tts == 0){
    stats <- c(stats, "tts_back_pct" = NA)
  } else {
    stats <- c(stats, "tts_back_pct" = stats[["tts_back"]]/tts)
  }
  
  # Left
  stats <- c(stats, "tts_left" = get_overlapping_duration("left", ss ,e))
  
  if(stats[["tts_left"]] == 0 | tts == 0){
    stats <- c(stats, "tts_left_pct" = NA)
  } else {
    stats <- c(stats, "tts_left_pct" = stats[["tts_left"]]/tts)
  }
  
  # Right
  stats <- c(stats, "tts_right" = get_overlapping_duration("right", ss ,e))
  
  if(stats[["tts_right"]] == 0 | tts == 0){
    stats <- c(stats, "tts_right_pct" = NA)
  } else {
    stats <- c(stats, "tts_right_pct" = stats[["tts_right"]]/tts)
  }
  
  # Stomach
  stats <- c(stats, "tts_stomach" = get_overlapping_duration("stomach", ss ,e))
  
  if(stats[["tts_stomach"]] == 0 | tts == 0){
    stats <- c(stats, "tts_stomach_pct" = NA)
  } else {
    stats <- c(stats, "tts_stomach_pct" = stats[["tts_stomach"]]/tts)
  }
  
  # Non-back
  stats <- c(stats, "tts_nonback" = get_overlapping_duration(c("right","stomach","left"), ss ,e))
  
  if(stats[["tts_nonback"]] == 0 | tts == 0){
    stats <- c(stats, "tts_nonback_pct" = NA)
  } else {
    stats <- c(stats, "tts_nonback_pct" = stats[["tts_nonback"]]/tts)
  }
  
  stats
}

#' Get snoring events related stats in a named vector.
#'
#' \code{snoring_stats} computes snoring related stats.
#'
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param ss sleep labels.
#' @return named vector
#' @examples
#' e <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
#' e$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
#' e$event = c("N3","N3","Train de ronflements")
#' snoring_stats(e)
#' @export
snoring_stats <- function(e , ss = c("N1","N2","N3","REM")){
  
  # Check events dataframe
  check_events(e)
  
  snorings <- get_overlapping_events(e,
                                     x = c("Train de ronflements"),
                                     y = ss)
  
  tts <- sum(as.numeric(difftime(e$end[e$event %in% ss],e$begin[e$event %in% ss],units="mins")))
  
  # snoring count
  stats <- c("snoring_count" = nrow(snorings))
  
  # snoring index
  stats <- c(stats, "snoring_idx" = stats[["snoring_count"]]/(tts/60))
  
  # Snoring duration total in minutes
  if(stats[["snoring_count"]] > 0){
    stats <- c(stats, "snoring_duration" = sum(as.numeric(difftime(snorings$end.x,snorings$begin.x,units="mins"))))
  } else {
    stats <- c(stats, "snoring_duration" = 0)
  }

  stats
}

#' Get Apnea and Hypopnea events related stats in a named vector.
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param ss sleep labels.
#' @param l label.
#' @return A named vector.
#' @export
resp_stats <- function(e, ss = c("N1","N2","N3","REM"), l = c("A. Obstructive", paste0("Hypopn","\u00E9","e"))){
  
  # Check events dataframe
  check_events(e)
  
  # Apnea and hypopnea count
  stats <- c("ah_count" = nrow(e[e$event %in% l,]))
  
  # Apnea and hypopnea index
  tts <- sum(as.numeric(difftime(e$end[e$event %in% ss], e$begin[e$event %in% ss], units="mins")))
  stats <- c(stats, "ah_idx" = stats[["ah_count"]]/(tts/60))
  
  # Apnea and hypopnea index in back position
  stats <- c(stats, "ah_idx_back" = nrow(get_overlapping_events(e,l,"back"))/(get_overlapping_duration("back",ss,e)/60))
  
  # AH non back
  stats <- c(stats, "ah_idx_nonback" = nrow(get_overlapping_events(e,l,c("right","left","stomach")))/(get_overlapping_duration(c("right","left","stomach"),ss,e)/60))
  
  # AH rem
  stats <- c(stats, "ah_idx_rem" = nrow(get_overlapping_events(e,l,"REM"))/(get_overlapping_duration("REM",ss,e)/60))
  
  # AH nonrem
  stats <- c(stats, "ah_idx_nrem" = nrow(get_overlapping_events(e,l,c("N1","N2","N3")))/(get_overlapping_duration(c("N1","N2","N3"),ss,e)/60))
  
  stats
}

#' Get Micro-Arousals events related stats in a named vector.
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param ss sleep labels.
#' @param l label.
#' @return A named vector.
#' @export
ma_stats <- function(e, ss = c("N1","N2","N3","REM"), l = paste0("Micro-","\u00E9","veil")){
  
  # Check events dataframe
  check_events(e)
  
  # Filtering sleep Micro-Arousals.
  ma <- get_overlapping_events(e,
                               x = c(l),
                               y = ss)
  
  # Micro-Arousals count.
  stats <- c("ma_count" = nrow(ma))
  
  # Micro-Arousals index.
  stats <- c(stats,"ma_idx" = 
              as.numeric(stats["ma_count"]/(sum(as.numeric(difftime(e$end[e$event %in% ss],e$begin[e$event %in% ss],units="mins")))/60))
               )
  
  # Micro-Arousals duration.
  stats <- c(stats,"ma_duration" = 
              sum(as.numeric(difftime(ma$end.x,ma$begin.x,units="mins")))
  )
  
  # Stages related Micro-Arousals stats
  for(s in ss){
    
    # Filtering sleep Micro-Arousals.
    ma <- get_overlapping_events(e,
                                 x = c(l),
                                 y = s)
    # Micro-Arousals count.
    r <- nrow(ma)
    names(r) <- paste0("ma_count_",s)
    stats <- c(stats, r)
    
    # Micro-Arousals index.
    r <- as.numeric(stats[paste0("ma_count_",s)]/(sum(as.numeric(difftime(e$end[e$event %in% s],e$begin[e$event %in% s],units="mins")))/60))
    names(r) <- paste0("ma_idx_",s)
    stats <- c(stats,r)
    
    # Micro-Arousals duration.
    r <- sum(as.numeric(difftime(ma$end.x,ma$begin.x,units="mins")))
    names(r) <- paste0("ma_duration_",s)
    stats <- c(stats,r)
    
  }
  
  stats
}

#' Get Rapid-Eye-Movements events related stats in a named vector.
#' @description Filters events to keep only Rapid-Eye-Movements. occuring during Rapid-Eye-Movements. sleep stage, accoring to the American Academy of Sleep Medicine scoring guidelines.\cr\cr 
#' Available statistics: 
#' \describe{
#'   \item{rem_count}{Count of Rapid-Eye-Movements.}
#'   \item{rem_index}{Rapid-Eye-Movements index by hour.}
#'   \item{rem_avg_duration}{Rapid-Eye-Movements average duration. Set to \code{NA} in the case no Rapid-Eye-Movements are found.}
#' }
#' @references Berry RB, Brooks R, Gamaldo CE, Harding SM, Lloyd RM, Marcus CL and Vaughn BV for the American Academy of Sleep Medicine. The AASM Manual for the Scoring of Sleep and Associated Events: Rules, Terminology and Technical Specifications, Version 2.2. www.aasmnet.org. Darien, Illinois: American Academy of Sleep Medicine, 2015.
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return A named vector.
#' @export
rem_stats <- function(e){
  
  # Check events dataframe
  check_events(e)
  
  # Filtering REM Rapid-Eye-Movements
  rem_rem <- get_overlapping_events(e,
                                    x = c("Rapide"),
                                    y = c("REM"))
  
  # Filtering REM stages
  rem_stages <- e[e$event == "REM", c("begin","end")]
  rem_duration <- sum(as.numeric(difftime(rem_stages$end,rem_stages$begin,units="min")))
  
  # REM count
  stats <- c("rem_count" = nrow(rem_rem))
  
  # REM index
  rem_index <- nrow(rem_rem)/(rem_duration/60)
  stats <- c(stats,"rem_count" = rem_index)
  
  # REM average duration
  rem_avg_duration <- ifelse( nrow(rem_rem) == 0, NA,
    mean(as.numeric(difftime(rem_rem$end.x,rem_rem$begin.x,units="secs"))))
  stats <- c(stats,"rem_avg_duration" = rem_avg_duration)
  
  stats
}

#' Get cycles related stats. Number of cycles, total duration, average duration.
#' 
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return A named vector.
#' @export
cycles_stats <- function(e){
  
  # Check events dataframe
  check_events(e)
  
  # Filtering cycles related events
  cycles_classic <- e[e$event == "cycle-CLASSIC",]
  cycles_begin <- e[e$event == "cycle-BN",]
  cycles_end <- e[e$event == "cycle-EN",]
  cycles_rem <- e[e$event == "cycle-REM",]
  
  # Number of cycles
  stats <- c("cycles_classic_count" = nrow(cycles_classic))
  stats <- c(stats, "cycles_begin_count" = nrow(cycles_begin))
  stats <- c(stats, "cycles_end_count" = nrow(cycles_end))
  stats <- c(stats, "cycles_rem_count" = nrow(cycles_rem))
  
  # Number of cycles
  stats <- c(stats,"cycles_classic_duration" = ifelse(
    nrow(cycles_classic) == 0, 0,
    sum(as.numeric(difftime(cycles_classic$end,cycles_classic$begin,units="min")))
  ))
  stats <- c(stats,"cycles_begin_duration" = ifelse(
    nrow(cycles_begin) == 0, 0,
    sum(as.numeric(difftime(cycles_begin$end,cycles_begin$begin,units="min")))
  ))
  stats <- c(stats,"cycles_rem_duration" = ifelse(
    nrow(cycles_rem) == 0, 0,
    sum(as.numeric(difftime(cycles_rem$end,cycles_rem$begin,units="min")))
  ))
  stats <- c(stats,"cycles_end_duration" = ifelse(
    nrow(cycles_end) == 0, 0,
    sum(as.numeric(difftime(cycles_end$end,cycles_end$begin,units="min")))
  ))
  
  stats <- c(stats, "cycles_classic_avg_duration" = ifelse(
    stats["cycles_classic_count"] == 0 || stats["cycles_classic_duration"] == 0, 0,
    stats["cycles_classic_count"]/stats["cycles_classic_duration"]
  ))
  
  stats <- c(stats, "cycles_begin_avg_duration" = ifelse(
    stats["cycles_begin_count"] == 0 || stats["cycles_begin_duration"] == 0, 0,
    stats["cycles_begin_count"]/stats["cycles_begin_duration"]
  ))
  stats <- c(stats, "cycles_classic_end_duration" = ifelse(
    stats["cycles_end_count"] == 0 || stats["cycles_end_duration"] == 0, 0,
    stats["cycles_end_count"]/stats["cycles_end_duration"]
  ))
  stats <- c(stats, "cycles_rem_avg_duration" = ifelse(
    stats["cycles_rem_count"] == 0 || stats["cycles_rem_duration"] == 0, 0,
    stats["cycles_rem_count"]/stats["cycles_rem_duration"]
  ))
  
  stats
}

#' Flattens a stage transition matrix to a named vector.
#' @description Flattens a transition matrix to a named vector to ensure stats functions consistency.
#' @param tm A transition matrix.
#' @return A named vector.
#' @export
tm_stats <- function(tm){
  ftm <- as.data.frame(tm)
  ftm$f <- row.names(ftm)
  row.names(ftm) <- NULL
  ftm <- stats::reshape(ftm,
                 direction = "long",
                 varying = list(names(ftm)[names(ftm) != "f"]),
                 v.names = "t",
                 idvar = "f",
                 timevar = "p",
                 times = names(ftm)[names(ftm) != "f"])
  ftm$label <- gsub("\\.","-",paste0("p_",row.names(ftm)))
  r <- ftm$t
  names(r) <- ftm$label
  r
}

#' Call and compile all statistics functions in a dataframe. 
#'
#' @param records record paths.
#' @param bands EEG bands to use. Ex: list(delta = c(0.5,3.5), theta = c(3.5,8), alpha = c(8,12),beta = c(12,30))
#' @param normalize Normalize band. Ex: c(0,40)
#' @param eeg_channels potential EEG channel names.
#' @param metadata read metadata or not.
#' @param butter Butterworth order.
#' @param resampling sRate to resample by.
#' @return df.
#' @export
compute_all_stats <- function(records,
                              bands,normalize,
                              eeg_channels = c("C3-A2","EEG Fpz-Cz","C3-M2","F4-M1"),
                              metadata = TRUE,
                              butter = FALSE,
                              resampling = FALSE){
  df <- data.frame(stringsAsFactors = FALSE)
  for(record in records){
    l <- read_mdf(mdfPath = record,channels = eeg_channels,metadata = metadata)
    df_record <- data.frame(stringsAsFactors = FALSE)
    if(length(l) > 0){
      for(eeg_channel in eeg_channels){
        
        if(nrow(df_record) == 0){
          if(eeg_channel %in% names(l[["channels"]])){
            
            hypnogram_band_powers <- sleepr::hypnogram_band_powers(record = l,
                                                                   channel = eeg_channel,
                                                                   bands = bands,
                                                                   normalize = normalize,
                                                                   butter = butter,
                                                                   resampling = resampling)
            df_record <- aggregate_band_powers(hypnogram_band_powers)
            df_record$eeg_channel <- eeg_channel
          }
        }
      }
      
      df_record$record <- record
      
      e <- l[["events"]]
      
      df_record <- cbind(df_record, as.data.frame(as.list(stages_stats(e))))
      df_record <- cbind(df_record, as.data.frame(as.list(ma_stats(e))))
      df_record <- cbind(df_record, as.data.frame(as.list(rem_stats(e))))
      df_record <- cbind(df_record, as.data.frame(as.list(cycles_stats(e))))
      df_record <- cbind(df_record, as.data.frame(as.list(tm_stats(tm(e)))))
      df_record <- cbind(df_record, as.data.frame(as.list(resp_stats(e))))
      df_record <- cbind(df_record, as.data.frame(as.list(snoring_stats(e))))
      df_record <- cbind(df_record, as.data.frame(as.list(pos_stats(e))))
      
    }
    df <- dplyr::bind_rows(df,df_record)
    
  }
  return(df)
  
}
