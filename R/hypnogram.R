#' Draw a histogram with ggplot.
#'
#' @param events events dataframe.
#' @return a ggplot.
plot_hypnogram <- function(events){
  stages <- hypnogram(events)
  stages$begin <- as.POSIXct(stages$begin)
  stages$end <- as.POSIXct(stages$end)
  hypnogram <- ggplot2::ggplot(stages,ggplot2::aes(x=begin,y=event,group=1)) +
    ggplot2::geom_line() + 
    ggplot2::theme_bw() +
    ggplot2::xlab("") + 
    ggplot2::ylab("")
  rem = stages[stages$event == "REM",]
  for(i in c(1:nrow(rem))){
    hypnogram <- hypnogram+ggplot2::geom_line(data=reshape2::melt(rem[i,],id.vars = c("event")),mapping = ggplot2::aes(x=value,y=event,group=1),colour='red')
  }
    
  return(hypnogram)
}

#' Filter an events dataframe to keep hypnogram.
#'
#' @param events events dataframe.
#' @return hypngram dataframe.
hypnogram <- function(events){
  labels <- c("N3","N2","N1","REM","AWA")
  stages <- events[events$event %in% labels,]
  stages$event <- factor(stages$event, levels = labels)
  stages <- stages[order(stages$begin),]
  return(stages)
}

#' Split signal following hypnogram
#'
#' @param signal signal.
#' @param hypnogram hypnogram df.
#' @param sRate sRate sig.
#' @return list.
split_signal <- function(signal,hypnogram,sRate){
  
  hypnogram$begin <- as.numeric(hypnogram$begin)
  hypnogram$end <- as.numeric(hypnogram$end)
  hypstart <- min(hypnogram$begin)
  hypnogram$begin <- (hypnogram$begin-hypstart)*sRate
  hypnogram$end <- (hypnogram$end-hypstart)*sRate
  
  splitted_signal <- list()
  for(i in c(1:nrow(hypnogram))){
    splitted_signal[[i]] <- signal[(hypnogram$begin[i]):(hypnogram$end[i])]
  }
  return(splitted_signal)
}


# record <- sleepr::read_mdf(mdfPath = "path/to/spec", channels = c("C3-M2"), metadata = FALSE)
# 
# spectrogram <- signal::specgram(
#   x = signal::resample(
#     x = record[["channels"]][["C3-M2"]][["signal"]],
#     q = record[["channels"]][["C3-M2"]][["metadata"]][["sRate"]],
#     p = 120),
#   Fs = 120,n = 2048, window = 1024, overlap = 512)
# 
# jpeg(filename = "test.jpeg",width = 1000,height = 500)
# spectrogram
# dev.off()