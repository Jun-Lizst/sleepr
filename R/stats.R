#' Call all available stats functions
#'
#' @param record record paths.
#' @param eeg_channels potential EEG channel names.
#' @return df.
compute_all_stats <- function(records, 
                              eeg_channels = c("C3-A2","EEG Fpz-Cz","C3-M2")){
  df <- data.frame(stringsAsFactors = FALSE)
  for(record in records){
    l <- sleepr::read.mdf(mdfPath = record)
    df_record <- data.frame(stringsAsFactors = FALSE)
    for(eeg_channel in eeg_channels){
      
      if(eeg_channel %in% names(l[["channels"]])){
        hypnogram_band_powers <- sleepr::hypnogram_band_powers(record = l,
                                                               channel = eeg_channel)
        hypnogram_band_powers$denominator <- NULL
        hypnogram_band_powers$broadband <- NULL
        hypnogram_band_powers$epoch <- NULL
        
        stages_means <- hypnogram_band_powers %>%
          dplyr::group_by(stage) %>%
          dplyr::summarise(delta = mean(delta),
                           theta = mean(theta),
                           alpha = mean(alpha),
                           beta = mean(beta),
                           gamma1 = mean(gamma1)) %>%
          reshape2::melt(id.vars = "stage") %>%
          dplyr::mutate(id = 1) %>%
          reshape2::dcast(id ~ stage+variable) %>%
          dplyr::select(-id)
        colnames(stages_means) <- paste0(colnames(stages_means),"_mean_",eeg_channel)
        df_record <- stages_means
      }
    }
    
    if(nrow(df_record) == 1){
      df_record$record <- record
    } else {
      df_record <- data.frame(record <- record,stringsAsFactors = FALSE)
    }
    
    df_record$rem_minutes <- get_rem_minutes(l[["events"]])
    df <- plyr::rbind.fill(df,df_record)
  }
  
  

  return(df)
}

#' Get total duration of REM sleep in minutes.
#'
#' @param events events dataframe.
#' @return total duration of REM sleep in minutes.
get_rem_minutes <- function(events){
  rem_events <- events[events$event == "REM", c("begin","end")]
  duration <- as.numeric(sum(rem_events$end - rem_events$begin))/60
  return(duration)
}

#' Get total duration of N1 sleep in minutes.
#'
#' @param events events dataframe.
#' @return total duration of N1 sleep in minutes.
get_n1_minutes <- function(events){
  n1_events <- events[events$event == "N1", c("begin","end")]
  duration <- as.numeric(sum(n1_events$end - n1_events$begin))/60
  return(duration)
}

