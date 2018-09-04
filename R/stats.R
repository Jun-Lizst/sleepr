#' Get total duration of REM sleep in minutes.
#'
#' @param events events dataframe.
#' @return total duration of REM sleep in minutes.
get_rem_minutes <- function(events){
  rem_events <- events[events$event == "REM", c("begin","end")]
  duration <- as.numeric(sum(rem_events$end - rem_events$begin))/60
  return(duration)
}
