#' Call all available stats functions
#'
#' @param record record paths.
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
    df_record$rem_minutes <- rem_minutes(l[["events"]])
    #df_record$awa_minutes <- get_awa_minutes(l[["events"]])
    df <- dplyr::bind_rows(df,df_record)
  }
  return(df)
}

#' Get total duration of REM sleep in minutes.
#'
#' @param hypnogram Hypnogram dataframe.
#' @return total duration of REM sleep in minutes.
rem_minutes <- function(hypnogram){
  rem_events <- hypnogram[hypnogram$event == "REM", c("begin","end")]
  return(sum(as.numeric(difftime(rem_events$end,rem_events$begin,units="secs"))/60))
}

#' Get total duration of N1 sleep in minutes.
#'
#' @param hypnogram Hypnogram dataframe.
#' @return total duration of N1 sleep in minutes.
n1_minutes <- function(hypnogram){
  n1_events <- hypnogram[hypnogram$event == "N1", c("begin","end")]
  return(sum(as.numeric(difftime(n1_events$end,n1_events$begin,units="secs"))/60))
}

#' Get total duration of N2 sleep in minutes.
#'
#' @param hypnogram Hypnogram dataframe.
#' @return total duration of N2 sleep in minutes.
n2_minutes <- function(hypnogram){
  n2_events <- hypnogram[hypnogram$event == "N2", c("begin","end")]
  return(sum(as.numeric(difftime(n2_events$end,n2_events$begin,units="secs"))/60))
}

#' Get total duration of N3 sleep in minutes.
#'
#' @param hypnogram Hypnogram dataframe.
#' @return total duration of N3 sleep in minutes.
n3_minutes <- function(hypnogram){
  n3_events <- hypnogram[hypnogram$event == "N3", c("begin","end")]
  return(sum(as.numeric(difftime(n3_events$end,n3_events$begin,units="secs"))/60))
}

#' Get total duration of AWA in minutes.
#'
#' @param hypnogram Hypnogram dataframe.
#' @return total duration of N3 sleep in minutes.
awa_minutes <- function(hypnogram){
  awa_events <- hypnogram[hypnogram$event == "AWA", c("begin","end")]
  return(sum(as.numeric(difftime(awa_events$end,awa_events$begin,units="secs"))/60))
}

#' time to sleep
#'
#' @param hypnogram Hypnogram dataframe.
#' @return tts in minutes.
tts <- function(hypnogram){
  events <- hypnogram[hypnogram$event %in% c("N1","N2","N3","REM"), c("begin","end")]
  return(sum(as.numeric(difftime(events$end,events$begin,units="secs"))/60))
}

rem_tts <- function(hypnogram){
  return(rem_minutes(hypnogram)/tts(hypnogram))
}

n3_tts <- function(hypnogram){
  return(n3_minutes(hypnogram)/tts(hypnogram))
}

n2_tts <- function(hypnogram){
  return(n2_minutes(hypnogram)/tts(hypnogram))
}

n1_tts <- function(hypnogram){
  return(n1_minutes(hypnogram)/tts(hypnogram))
}

pts <- function(hypnogram){
  return(as.numeric(difftime(max(hypnogram$end),min(hypnogram$begin),units="secs"))/60)
}

sleep_efficiency <- function(hypnogram){
  return(tts(hypnogram)/pts(hypnogram))
}

sleep_latency <- function(hypnogram){
  sleep <- hypnogram[hypnogram$event %in% c("N1","N2","N3","REM"),]
  wake <- hypnogram[hypnogram$event == "AWA",]
  return(as.numeric(difftime(min(sleep$begin),min(wake$begin),units="secs"))/60)
}

rem_latency <- function(hypnogram){
  rem <- hypnogram[hypnogram$event == "REM",]
  return(as.numeric(difftime(min(rem$begin),min(hypnogram$begin),units="secs"))/60-sleep_latency(hypnogram))
}

wake_after_onset <- function(hypnogram){
  return(pts(hypnogram)-sleep_latency(hypnogram)-tts(hypnogram))
}

tts_pos_back <- function(events){
  es <- events[events$event == "Dos",] # [e]vents [s]ubset 
  h <- hypnogram(events)
  td <- 0
  for(i in c(1:nrow(es))){
    ed <- 0
    hes <- h[(h$begin<=es[i,]$begin & h$end>es[i,]$begin)
             | (h$begin>=es[i,]$begin & h$end<=es[i,]$end)
             | (h$begin<=es[i,]$end & h$end>=es[i,]$end),]
    for(j in c(1:nrow(hes))){
      if(hes$event[j] != "AWA"){
        hi <- lubridate::interval(hes$begin[j], hes$end[j])
        esi <- lubridate::interval(es$begin[i], es$end[i])
        ed <- ed + as.numeric(
          lubridate::as.duration(
            lubridate::intersect(
              hi, esi)),"seconds")
      }
    }
    td <- td+ed
  }
  return(td/60)
}

# tts_pos_back_pct <- function(events){
#   return(tts_pos_back(events)/tts(events))
# }
#   
# tts_pos_back_pct(mdf[["events"]])
# plot_hypnogram(hypnogram(mdf[["events"]]))
# mdf <- read_mdf("/Users/paul/dariot/",channels = "C3-M2")
# # stats <- sleepr::compute_all_stats("/Users/paul/20180718T013310/",
# #                                    eeg_channels = "C3-M2",
# #                                    metadata = TRUE)
#hypnogram <- hypnogram(mdf[["events"]])
