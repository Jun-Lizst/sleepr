#' Call all available stats functions
#'
#' @param record record paths.
#' @return df.
compute_all_stats <- function(records){
  
  df <- data.frame(record = character(),
                   rem_minutes = numeric(),
                   stringsAsFactors = FALSE)
  colnames <- c("record","rem_minutes")
  for(record in records){
    l <- sleepr::read.mdf(record,c())
    df[nrow(df)+1,] <- list(record,
                            get_rem_minutes(l[["events"]]))
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

