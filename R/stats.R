#' Call all available stats functions
#'
#' @param record record paths.
#' @param eeg_channels potential EEG channel names.
#' @return df.
compute_all_stats <- function(records, 
                              eeg_channels = c("C3-A2","EEG Fpz-Cz")){
  df <- data.frame(stringsAsFactors = FALSE)
  for(record in records){
    l <- read_mdf(mdfPath = record)
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
    df_record$rem_minutes <- get_rem_minutes(l[["events"]])
    #df_record$awa_minutes <- get_awa_minutes(l[["events"]])
    df <- dplyr::bind_rows(df,df_record)
  }
  return(df)
}

#' Get total duration of REM sleep in minutes.
#'
#' @param hypnogram Hypnogram dataframe.
#' @return total duration of REM sleep in minutes.
get_rem_minutes <- function(hypnogram){
  rem_events <- hypnogram[hypnogram$event == "REM", c("begin","end")]
  duration <- as.numeric(sum(rem_events$end - rem_events$begin))/60
  return(duration)
}

#' Get total duration of N1 sleep in minutes.
#'
#' @param hypnogram Hypnogram dataframe.
#' @return total duration of N1 sleep in minutes.
get_n1_minutes <- function(hypnogram){
  n1_events <- hypnogram[hypnogram$event == "N1", c("begin","end")]
  duration <- as.numeric(sum(n1_events$end - n1_events$begin))/60
  return(duration)
}

#' Get total duration of N2 sleep in minutes.
#'
#' @param hypnogram Hypnogram dataframe.
#' @return total duration of N2 sleep in minutes.
get_n2_minutes <- function(hypnogram){
  n2_events <- hypnogram[hypnogram$event == "N2", c("begin","end")]
  duration <- as.numeric(sum(n2_events$end - n2_events$begin))/60
  return(duration)
}

#' Get total duration of N3 sleep in minutes.
#'
#' @param hypnogram Hypnogram dataframe.
#' @return total duration of N3 sleep in minutes.
get_n3_minutes <- function(hypnogram){
  n3_events <- hypnogram[hypnogram$event == "N3", c("begin","end")]
  duration <- as.numeric(sum(n3_events$end - n3_events$begin))/60
  return(duration)
}

#' Record total duration in minutes.
#'
#' @param hypnogram Hypnogram dataframe.
#' @return total total duration in minutes.
# get_recording_duration <- function(hypnogram){
#   
#   duration <- as.numeric(sum(n3_events$end - n3_events$begin))/60
#   return(duration)
# }


