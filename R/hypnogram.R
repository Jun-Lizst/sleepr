#' Draw a histogram with ggplot.
#'
#' @param events events dataframe.
#' @return a ggplot.
plot.hypnogram <- function(events){
  stages <- sleepr::get.hypnogram(events)
  stages$begin <- as.POSIXct(stages$begin)
  hypnogram <- ggplot2::ggplot(stages,ggplot2::aes(x=begin,y=event, group=1)) +
    ggplot2::geom_line() + 
    ggplot2::theme_bw() +
    ggplot2::xlab("") + 
    ggplot2::ylab("")
  return(hypnogram)
}

#' Filter an events dataframe to keep hypnogram.
#'
#' @param events events dataframe.
#' @return hypngram dataframe.
get.hypnogram <- function(events){
  labels <- c("N3","N2","N1","REM","AWA")
  stages <- events[events$event %in% labels,]
  stages$event <- factor(stages$event, levels = labels)
  stages <- stages[order(stages$begin),]
  return(stages)
}