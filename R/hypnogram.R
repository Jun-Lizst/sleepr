#' Draw a histogram with ggplot2.
#'
#' @param events events dataframe.
#' @param labels hyp labels.
#' @return a ggplot.
#' @export
plot_hypnogram <- function(events, labels = c("N3","N2","N1","REM","AWA")){
  stages <- hypnogram(events, labels)
  stages$begin <- as.POSIXct(stages$begin)
  stages$end <- as.POSIXct(stages$end)
  hypnogram <- ggplot2::ggplot(stages,ggplot2::aes_string(x="begin",y="event",group=1)) +
    ggplot2::geom_line() + 
    ggplot2::theme_bw() +
    ggplot2::xlab("") + 
    ggplot2::ylab("")
  rem = stages[stages$event == "REM",]
  if(nrow(rem) > 0){
    for(i in c(1:nrow(rem))){
      df <- stats::reshape(rem[i,], idvar = "event", varying = c("begin","end"),
                           v.names = "value", direction = "long")
      hypnogram <- hypnogram+ggplot2::geom_line(data=df,mapping = ggplot2::aes_string(x="value",y="event",group=1),colour='red')
    }
  }
  return(hypnogram)
}

#' Filter an events dataframe to keep hypnogram.
#'
#' @param events events dataframe.
#' @param labels events labels in data.
#' @return hypnogram dataframe.
#' @export
hypnogram <- function(events, labels = c("N3","N2","N1","REM","AWA")){
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
#' @export
split_signal <- function(signal,hypnogram,sRate){
  
  hypnogram$begin <- as.numeric(hypnogram$begin)
  hypnogram$end <- as.numeric(hypnogram$end)
  hypstart <- min(hypnogram$begin)
  hypnogram$begin <- (hypnogram$begin-hypstart)*sRate+1
  hypnogram$end <- (hypnogram$end-hypstart)*sRate
  
  splitted_signal <- list()
  for(i in c(1:nrow(hypnogram))){
    splitted_signal[[i]] <- signal[(hypnogram$begin[i]):(hypnogram$end[i])]
  }
  return(splitted_signal)
}

#' Get spectrogram
#'
#' @param signal signal.
#' @param sRate sRate.
#' @param maxfreq maxfreq.
#' @param windowlength windowlength.
#' @param show show.
#' @return list.
#' @export
plot_spectrogram <- function(signal, sRate, maxfreq = 25, windowlength = 2000, show = FALSE){
  phonTools::spectrogram(signal, fs = sRate, windowlength = windowlength, maxfreq = maxfreq, show = show)
}




