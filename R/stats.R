#' Call and compile all statistics functions in a dataframe. 
#'
#' @param records record paths.
#' @param eeg_channels potential EEG channel names.
#' @param metadata read metadata or not.
#' @return df.
compute_all_stats <- function(records, 
                              eeg_channels = c("C3-A2","EEG Fpz-Cz"),
                              metadata = TRUE){
  df <- data.frame(stringsAsFactors = FALSE)
  for(record in records){
    l <- read_mdf(mdfPath = record,channels = eeg_channels,metadata = metadata)
    df_record <- data.frame(stringsAsFactors = FALSE)
    if(length(l) > 0){
      for(eeg_channel in eeg_channels){
        
        if(nrow(df_record) == 0){
          if(eeg_channel %in% names(l[["channels"]])){
            
            hypnogram_band_powers <- sleepr::hypnogram_band_powers(record = l,
                                                                   channel = eeg_channel)
            hypnogram_band_powers$denominator <- NULL
            hypnogram_band_powers$broadband <- NULL
            hypnogram_band_powers$epoch <- NULL
            
            df_record <- hypnogram_band_powers
            df_record <-   dplyr::group_by(df_record,stage)
            df_record <-  dplyr::summarise(df_record,
                                           delta = mean(delta),
                                           theta = mean(theta),
                                           alpha = mean(alpha),
                                           beta = mean(beta),
                                           gamma1 = mean(gamma1))
            df_record <-  reshape2::melt(df_record,id.vars = "stage")
            df_record <-  dplyr::mutate(df_record,id = 1)
            df_record <-  reshape2::dcast(df_record,id ~ stage+variable)
            df_record <- dplyr::select(df_record,-id)
            colnames(df_record) <- paste0(tolower(colnames(df_record)),"_mean_eeg")
          }
        }
      }
      if(nrow(df_record) == 1){
        df_record$record <- record
      } else {
        df_record <- data.frame(record <- record,stringsAsFactors = FALSE)
      }
      
      e <- l[["events"]]
      df_record$rem_duration <- rem_duration(l[["events"]])
      df_record$n1_duration <- n1_duration(l[["events"]])
      df_record$n2_duration <- n2_duration(hypnogram(l[["events"]]))
      df_record$n3_duration <- n3_duration(hypnogram(l[["events"]]))
      df_record$awa_duration <- awa_duration(hypnogram(l[["events"]]))
      df_record$tts <- tts(hypnogram(l[["events"]]))
      df_record$rem_tts <- rem_tts(hypnogram(l[["events"]]))
      df_record$n3_tts <- n3_tts(hypnogram(l[["events"]]))
      df_record$n2_tts <- n2_tts(hypnogram(l[["events"]]))
      df_record$n1_tts <- n1_tts(hypnogram(l[["events"]]))
      df_record$tsp <- tsp(hypnogram(l[["events"]]))
      df_record$sleep_efficiency <- sleep_efficiency(hypnogram(l[["events"]]))
      df_record$sleep_latency <- sleep_latency(hypnogram(l[["events"]]))
      df_record$rem_latency <- rem_latency(hypnogram(l[["events"]]))
      df_record$waso <- waso(hypnogram(l[["events"]]))
      df_record$tts_pos_back <- tts_pos_back(l[["events"]])
      df_record$tts_pos_back_pct <- tts_pos_back_pct(l[["events"]])
      df_record$tts_pos_left <- tts_pos_left(l[["events"]])
      df_record$tts_pos_left_pct <- tts_pos_left_pct(l[["events"]])
      df_record$tts_pos_stomach <- tts_pos_stomach(l[["events"]])
      df_record$tts_pos_stomach_pct <- tts_pos_stomach_pct(l[["events"]])
      df_record$tts_pos_right <- tts_pos_right(l[["events"]])
      df_record$tts_pos_right_pct <- tts_pos_right_pct(l[["events"]])
      df_record$tts_pos_nonback <- tts_pos_nonback(l[["events"]])
      df_record$tts_pos_nonback_pct <- tts_pos_nonback_pct(l[["events"]])
      df_record$ah_count <- ah_count(l[["events"]])
      df_record$ah_hour <- ah_hour(l[["events"]])
      df_record$ah_back <- ah_back(l[["events"]])
      df_record$ah_nonback <- ah_nonback(l[["events"]])
      df_record$ah_rem <- ah_rem(l[["events"]])
      df_record$ah_nonrem <- ah_nonrem(l[["events"]])
      
      # Micro-arousals
      df_record$ma_count <- ma_count(e)
      df_record$ma_index <- ma_index(e)
      df_record$ma_duration <- ma_duration(e)
      df_record$ma_n1_duration <- ma_n1_duration(e)
      df_record$ma_n2_duration <- ma_n2_duration(e)
      df_record$ma_n3_duration <- ma_n3_duration(e)
      df_record$ma_rem_duration <- ma_rem_duration(e)
      df_record$ma_n1_count <- ma_n1_count(e)
      df_record$ma_n2_count <- ma_n2_count(e)
      df_record$ma_n3_count <- ma_n3_count(e)
      df_record$ma_rem_count <- ma_rem_count(e)
      df_record$ma_n1_index <- ma_n1_index(e)
      df_record$ma_n2_index <- ma_n2_index(e)
      df_record$ma_n3_index <- ma_n3_index(e)
      df_record$ma_rem_index <- ma_rem_index(e)
      
      # Rapid Eye Movements
      df_record$rem_count <- rem_count(e)
      df_record$rem_index <- rem_index(e)
      df_record$rem_avg_duration <- rem_avg_duration(e)
    }
      df <- dplyr::bind_rows(df,df_record)
       
  }
  return(df)
    
}


#' Filter events overlapping specified events.
#'
#' @param events Events.
#' @param x Events labels overlapping.
#' @param y Events labels to be overlapped.
#' @return Dataframe of x events overlapping y events.
get_overlapping_events <- function(events, x, y){
  x <- events[events$event %in% x,]
  y <- events[events$event %in% y,]
  
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


events_stages_overlap <- function(label, stages, events){
  es <- events[events$event %in% label,] # [e]vents [s]ubset 
  h <- hypnogram(events)
  td <- 0
  for(i in c(1:nrow(es))){
    ed <- 0
    hes <- h[(h$begin<=es[i,]$begin & h$end>es[i,]$begin)
             | (h$begin>=es[i,]$begin & h$end<=es[i,]$end)
             | (h$begin<=es[i,]$end & h$end>=es[i,]$end),]
    if(nrow(hes) > 0 & nrow(h) > 0){
      for(j in c(1:nrow(hes))){
        if(hes$event[j] %in% stages){
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

check_events_integrity <- function(events){
  if(nrow(events) == 0 ){
    warning("Events dataframe is empty.")
    return(FALSE)
  } else if(!("begin" %in% colnames(events))){
    warning("Events dataframe must contain a 'begin' column.")
    return(FALSE)
  } else if(!("end" %in% colnames(events))){
    warning("Events dataframe must contain a 'end' column.")
    return(FALSE)
  } else  if(!("event" %in% colnames(events))){
    warning("Events dataframe must contain a 'event' column.")
    return(FALSE)
  } else if(!("POSIXt" %in% class(events$begin))){
    warning("'begin' column must be a datetime.")
    return(FALSE)
  } else if(!("POSIXt" %in% class(events$end))){
    warning("'end' column must be a datetime.")
    return(FALSE)
  } else if(!("character" %in% class(events$event))){
    events$event <- as.character(events$event)
    return(TRUE)
  } else {
    return(TRUE)
  }
}

# Stages & scoring ----

#' REM sleep duration in minutes.
#'
#' \code{rem_duration} sums up REM stages duration from an events dataframe to get total REM duration in minutes.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return Total duration of REM sleep in minutes.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("REM","REM")
#' rem_duration(events)
rem_duration <- function(events){
  if(!check_events_integrity(events)){ return(0) }
  events <- events[events$event == "REM", c("begin","end")]
  return(sum(as.numeric(difftime(events$end,events$begin,units="min"))))
}

#' N1 sleep duration in minutes.
#'
#' \code{n1_duration} sums up N1 stages duration from an events dataframe to get total N1 duration in minutes.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return Total duration of N1 sleep in minutes.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N1","N1")
#' n1_duration(events)
n1_duration <- function(events){
  if(!check_events_integrity(events)){ return(0) }
  events <- events[events$event == "N1", c("begin","end")]
  return(sum(as.numeric(difftime(events$end,events$begin,units="min"))))
}

#' N2 sleep duration in minutes.
#'
#' \code{n2_duration} up N2 stages duration from an events dataframe to get total N2 duration in minutes.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return total duration of N2 sleep in minutes.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N2","N2")
#' n2_duration(events)
n2_duration <- function(events){
  if(!check_events_integrity(events)){ return(0) }
  n2_events <- events[events$event == "N2", c("begin","end")]
  return(sum(as.numeric(difftime(n2_events$end,n2_events$begin,units="mins"))))
}

#' N3 sleep duration in minutes.
#'
#' \code{n3_duration} sums up N3 stages duration from an events dataframe to get total N3 duration in minutes.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return total duration of N3 sleep in minutes.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N3","N3")
#' n3_duration(events)
n3_duration <- function(events){
  if(!check_events_integrity(events)){ return(0) }
  n3_events <- events[events$event == "N3", c("begin","end")]
  return(sum(as.numeric(difftime(n3_events$end,n3_events$begin,units="mins"))))
}

#' Wake duration in minutes.
#' 
#' \code{awa_duration} sums up AWA stages duration from an events dataframe to get total AWA duration in minutes.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return total duration of AWA sleep in minutes.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("AWA","AWA")
#' awa_duration(events)
awa_duration <- function(events){
  if(!check_events_integrity(events)){ return(0) }
  awa_events <- events[events$event == "AWA", c("begin","end")]
  return(sum(as.numeric(difftime(awa_events$end,awa_events$begin,units="mins"))))
}

#' Time To Sleep in minutes.
#' 
#' \code{tts} (Time To Sleep) sums up REM, N1, N2 and N3 stages duration from an events dataframe to get Time To Sleep duration in minutes.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return Time To Sleep (N1+N2+N3+REM durations) in minutes.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N1","REM")
#' tts(events)
tts <- function(events){
  if(!check_events_integrity(events)){ return(0) }
  events <- events[events$event %in% c("N1","N2","N3","REM"), c("begin","end")]
  return(sum(as.numeric(difftime(events$end,events$begin,units="mins"))))
}

#' REM over TTS ratio.
#' 
#' Divides REM duration by TTS duration from an events dataframe.
#'
#' @param events Events dataframe. Must contain begin, end and events.
#' @return REM over TTS durations ratio.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N1","REM")
#' rem_tts(events)
rem_tts <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  return(rem_duration(events)/tts(events))
}

#' Divides N3 duration by TTS duration from an events dataframe.
#'
#' @param events Events dataframe. Must contain begin, end and events.
#' @return N3 over TTS durations ratio.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N3","REM")
#' n3_tts(events)
n3_tts <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  return(n3_duration(events)/tts(events))
}

#' Divides N2 duration by TTS duration from an events dataframe.
#'
#' @param events Events dataframe. Must contain begin, end and events.
#' @return N2 over TTS durations ratio.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N3","N2")
#' n2_tts(events)
n2_tts <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  return(n2_duration(events)/tts(events))
}

#' Divides N1 duration by TTS duration from an events dataframe.
#'
#' @param events Events dataframe. Must contain begin, end and events.
#' @return N1 over TTS durations ratio.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N3","N1")
#' n1_tts(events)
n1_tts <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  return(n1_duration(events)/tts(events))
}

#' Substracts the end time of the last element to the begin time of the first element to get the Total Sleep Period in minutes.
#'
#' @param events Events dataframe. Must contain begin, end and events.
#' @return Total Sleep Period in minutes.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N3","N1")
#' tsp(events)
tsp <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  return(as.numeric(difftime(max(events$end),min(events$begin),units="mins")))
}

#' Divides the Time To Sleep (TTS) by the Total Sleep Period (TSP) to get the sleep efficiency ratio.
#' 
#' @param events Events dataframe. Must contain begin, end and events.
#' @return sleep efficiency ratio.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N3","AWA")
#' sleep_efficiency(events)
sleep_efficiency <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  return(tts(events)/tsp(events))
}

#' Substracts the start time of the first sleep epoch (N1,N2,N3 or REM) to the start of the hypnogram.
#' 
#' @param events Events dataframe. Must contain begin, end and events.
#' @return Sleep latency.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("AWA","N3")
#' sleep_latency(events)
sleep_latency <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  sleep <- events[events$event %in% c("N1","N2","N3","REM"),]
  return(as.numeric(difftime(min(sleep$begin),min(events$begin),units="mins")))
}

#' REM Latency in minutes.
#' 
#' Substracts the start time of the first REM epoch to the start of the sleep onset to get the REM latency in minutes.
#' 
#' @param events Events dataframe. Must contain begin, end and events.
#' @return REM Latency in minutes.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("AWA","REM")
#' rem_latency(events)
#' 
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("REM","REM")
#' rem_latency(events)
#' 
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N1","REM")
#' rem_latency(events)
rem_latency <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  rem <- events[events$event == "REM",]
  return(as.numeric(difftime(min(rem$begin),min(events$begin),units="mins"))-sleep_latency(events))
}

#' Wake After Sleep Onset in minutes.
#'
#' \code{waso} substracts Time To Sleep (TTS) and sleep latency to the Total Sleep Period (TSP) to get the total Wake After Sleep Onset in minutes.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return Wake After Sleep Onset in minutes.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860), origin = "1970-01-01")
#' events$event = c("N2","AWA")
#' waso(events)
waso <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  return(tsp(events)-sleep_latency(events)-tts(events))
}

# Position & activity ----

#' TTS duration in back position in minutes.
#'
#' \code{tts_pos_back} computes the total time in back position during TTS.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return TTS duration in back position in minutes.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
#' events$event = c("N3","N3","back")
#' tts_pos_back(events)
tts_pos_back <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  return(events_stages_overlap("back",c("N1","N2","N3","REM"),events))
}

#' TTS duration in back position over TTS duration.
#'
#' \code{tts_pos_back_pct} computes the total time in back position during TTS over TTS duration.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return TTS duration in back position over TTS
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
#' events$event = c("N3","N3","back")
#' tts_pos_back_pct(events)
tts_pos_back_pct <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  return(tts_pos_back(events)/tts(events))
}

#' TTS duration in left position in minutes.
#'
#' \code{tts_pos_left} computes the total time in left position during TTS in minutes.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return TTS duration in left position.
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
#' events$event = c("N3","N3","left")
#' tts_pos_left(events)
tts_pos_left <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  return(events_stages_overlap("left",c("N1","N2","N3","REM"),events))
}

#' TTS duration in left position over TTS duration.
#'
#' \code{tts_pos_back_pct} computes the total time in left position during TTS over TTS duration.
#'
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @return TTS duration in back position over TTS
#' @examples
#' events <- data.frame(begin = as.POSIXlt(c(1536967800,1536967830,1536967810),origin = "1970-01-01"))
#' events$end <- as.POSIXlt(c(1536967830,1536967860,1536967820), origin = "1970-01-01")
#' events$event = c("N3","N3","back")
#' tts_pos_left_pct(events)
tts_pos_left_pct <- function(events){
  if(!check_events_integrity(events)){ return(NA) }
  return(tts_pos_left(events)/tts(events))
}

tts_pos_stomach <- function(events){
  return(events_stages_overlap("Ventre",c("N1","N2","N3","REM"),events))
}

tts_pos_stomach_pct <- function(events){
  return(tts_pos_stomach(events)/tts(events))
}

tts_pos_right <- function(events){
  return(events_stages_overlap("Droite",c("N1","N2","N3","REM"),events))
}

tts_pos_right_pct <- function(events){
  return(tts_pos_right(events)/tts(events))
}

tts_pos_nonback <- function(events){
  return(tts(hypnogram(events))-tts_pos_back(events))
}

tts_pos_nonback_pct <- function(events){
  return(tts_pos_nonback(events)/tts(events))
}

# Snoring ----

# Number of snorings
snoring_count <- function(events){
  return(nrow(get_overlapping_events(events,
                         x = c("Train de ronflements"),
                         y = c("N1","N2","N3","REM"))
  ))
}

# Snorings per hour
snoring_index <- function(events){
  return(snoring_count(events)/(tts(hypnogram(events))/60))
}

snoring_duration <- function(events){
  s <- get_overlapping_events(events,
                         x = c("Train de ronflements"),
                         y = c("N1","N2","N3","REM"))
  s$duration <- as.numeric(difftime(s$end.x,s$begin.x,units="secs"))/60
  return(sum(s$duration))
}

snoring_duration_pct <- function(events){
  return(snoring_duration(events)/tts(hypnogram(events)))
}

# Respiratory indexes ----

ah_count <- function(events){
  return(nrow(events[events$event %in%
                       c("A. Obstructive",
                         "Hypopnée"),]))
}

ah_hour <- function(events){
  return(ah_count(events)/(tts(hypnogram(events))/60))
}

ah_back <- function(events){
  return(nrow(get_overlapping_events(events,c("Hypopnée","A. Obstructive"),"back"))/(tts_pos_back(events)/60))
}

ah_nonback <- function(events){
  return(nrow(get_overlapping_events(events,c("Hypopnée","A. Obstructive"),
                                     c("Droite","Gauche","Ventre")))/(tts_pos_nonback(events)/60))
}

ah_rem <- function(events){
  return(nrow(get_overlapping_events(events,c("Hypopnée","A. Obstructive"),
                                     c("REM")))/(rem_duration(hypnogram(events))/60))
}

ah_nonrem <- function(events){
  return(
    nrow(
      get_overlapping_events(
        events,
        c("Hypopnée",
          "A. Obstructive"),
        c("N1","N2","N3")))/
      ((n1_duration(hypnogram(events))+
          n2_duration(hypnogram(events))+
          n3_duration(hypnogram(events)))/60)
    )
}

# Oxygen stauration ----

# Pulse ----

# Quality ----

# PLM ----

# Micro-arousals ----

# Count
ma_count <- function(events){
  return(nrow(get_overlapping_events(events,
                                     x = c("micro-arousal"),
                                     y = c("N1","N2","N3","REM"))
  ))
}

# Per hour of tts
ma_index <- function(events){
  return(ma_count(events)/(tts(events)/60))
}

ma_duration <- function(events){
  events <- get_overlapping_events(events,
                                   x = c("micro-arousal"),
                               y = c("N1","N2","N3","REM"))
  if(nrow(events) == 0){
    return(0)
  } 
  duration <- sum(as.numeric(difftime(events$end.x,events$begin.x,units="mins")))
  return(duration)
}

ma_n1_duration <- function(events){
  events <- get_overlapping_events(events,
                                   x = c("micro-arousal"),
                                   y = c("N1"))
  if(nrow(events) == 0){
    return(0)
  } 
  duration <- sum(as.numeric(difftime(events$end.x,events$begin.x,units="mins")))
  return(duration)
}

ma_n2_duration <- function(events){
  events <- get_overlapping_events(events,
                                   x = c("micro-arousal"),
                                   y = c("N2"))
  if(nrow(events) == 0){
    return(0)
  } 
  duration <- sum(as.numeric(difftime(events$end.x,events$begin.x,units="mins")))
  return(duration)
}

ma_n3_duration <- function(events){
  events <- get_overlapping_events(events,
                                   x = c("micro-arousal"),
                                   y = c("N3"))
  if(nrow(events) == 0){
    return(0)
  } 
  duration <- sum(as.numeric(difftime(events$end.x,events$begin.x,units="mins")))
  return(duration)
}

ma_rem_duration <- function(events){
  events <- get_overlapping_events(events,
                                   x = c("micro-arousal"),
                                   y = c("REM"))
  if(nrow(events) == 0){
    return(0)
  } 
  duration <- sum(as.numeric(difftime(events$end.x,events$begin.x,units="mins")))
  return(duration)
}

ma_n1_count <- function(events){
  return(nrow(get_overlapping_events(events,
                                     x = c("micro-arousal"),
                                     y = c("N1"))
  ))
}

ma_n2_count <- function(events){
  return(nrow(get_overlapping_events(events,
                                     x = c("micro-arousal"),
                                     y = c("N2"))
  ))
}

ma_n3_count <- function(events){
  return(nrow(get_overlapping_events(events,
                                     x = c("micro-arousal"),
                                     y = c("N3"))
  ))
}

ma_rem_count <- function(events){
  return(nrow(get_overlapping_events(events,
                                     x = c("micro-arousal"),
                                     y = c("REM"))
  ))
}

ma_n1_index <- function(events){
  return(ma_n1_count(events)/(n1_duration(events)/60))
}

ma_n2_index <- function(events){
  return(ma_n2_count(events)/(n2_duration(events)/60))
}

ma_n3_index <- function(events){
  return(ma_n3_count(events)/(n3_duration(events)/60))
}

ma_rem_index <- function(events){
  return(ma_rem_count(events)/(rem_duration(events)/60))
}

# Rapid Eye Movements ----

rem_count <- function(events){
  events <- get_overlapping_events(events,
                                   x = c("Rapide"),
                                   y = c("REM"))
  return(nrow(events))
}

rem_index <- function(events){
  return(rem_count(events)/(rem_duration(events)/60))
}

rem_avg_duration <- function(events){
  
  
  events <- get_overlapping_events(events,
                                   x = c("Rapide"),
                                   y = c("REM"))
  if(nrow(events) == 0){
    return(0)
  }
  return(mean(as.numeric(difftime(events$end.x,events$begin.x,units="secs"))))
}

# Cycles ----

normalize_cycles <- function(events){
  events <- events[events$event %in% c("Activity-CLASSICstart",
                                       "Activity-CLASSICend",
                                       "Activity-REMstart",
                                       "Activity-REMend",
                                       "Activity-ENstart",
                                       "Activity-ENend"),]
  events <- events[order(events$begin),]
}











# events <- read_events_noxturnal("tests/testthat/data/noxturnal_events_example_unicode_3.csv")
# unique(events$event)
