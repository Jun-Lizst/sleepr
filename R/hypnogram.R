#' Draw a hypnogram with ggplot2.
#'
#' @references Silber MH, Ancoli-Israel S, Bonnet MH, Chokroverty S, Grigg-Damberger MM, et al. (2007). "The visual scoring of sleep in adults". Journal of Clinical Sleep Medicine. 3 (2): 121–31. PMID 17557422
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

#' Split signal into epochs
#'
#' @param signal signal.
#' @param sRate sRate sig.
#' @param duration in seconds.
#' @return list.
#' @export
split_epochs <- function(signal, sRate, duration = 30){
  epoch_length <- sRate * duration
  epochs <- split(signal, ceiling(seq_along(signal)/epoch_length))
  return(epochs)
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

#' Get transitions graph
#'
#' @param e hypnogram
#' @param height height
#' @param width width
#' @return list.
#' @export
plot_transitions <- function(e, height = "500px", width = "100%"){
  
  if(!check_events_integrity(e)){ return(NA) }
  
  # Hypnogram
  h <- hypnogram(e)
  h$event <- as.character(h$event)
  
  hc <- data.frame(table(h$event))
  colnames(hc) <- c("event","value")
  
  h <- merge(h,hc, by = "event")
  h <- h[order(h$begin),]
  
  # nodes
  nodes <- data.frame(id = 1:length(unique(h$event)),
                      shape = "circle",
                      event = unique(h$event),
                      stringsAsFactors = FALSE)
  nodes <- merge(unique(h[,c("event","value")]),nodes)
  nodes$label <- paste0(nodes$event," (",nodes$value,")")
  nodes$group <- nodes$label
  nodes$value <- NULL
  
  # edges
  h$n <- c(h$event[-1],NA)
  h$c <- 1
  c <- stats::aggregate(c ~ event + n, data = h, FUN = sum)
  c <- merge(c, hc, by = "event")
  c$p <- c$c/c$value
  
  edges <- c
  edges$label <- round(edges$p, digits = 3)
  edges <- merge(edges,nodes[,c("event","id")], by = "event")
  edges$from <- edges$id
  edges$id <- NULL
  edges$event <- edges$n
  edges <- merge(edges,nodes[,c("event","id")], by = "event")
  edges$to <- edges$id
  edges <- edges[,c("from","to","label")]
  edges$length <- 500
  edges$width <- 2
  edges$arrows <- "to"
  
  visNetwork::visNetwork(nodes, edges, height = height, width = width)
}
