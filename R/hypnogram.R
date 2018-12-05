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
#' @param hypnogram hypnogram
#' @param height height
#' @param width width
#' @return list.
#' @export
plot_transitions <- function(hypnogram, height = "500px", width = "100%"){
  
  # Hypnogram
  hypnogram <- hypnogram(hypnogram)
  hypnogram$event <- as.character(hypnogram$event)
  hypnogram_counts <- data.frame(table(hypnogram$event))
  colnames(hypnogram_counts) <- c("event","count")
  hypnogram <- merge(hypnogram,hypnogram_counts, by = "event")
  hypnogram <- hypnogram[order(hypnogram$begin),]
  hypnogram$event <- paste0(hypnogram$event," (",hypnogram$count,")")
  #hypnogram$count <- NULL
  hypnogram$nextstage <- c(hypnogram$event[-1],NA)
  
  transitions <- nrow(hypnogram[hypnogram$nextstage != hypnogram$event,c("nextstage","event")])
  
  # Nodes
  nodes <- data.frame(id = 1:length(unique(hypnogram$event)), 
                      group = unique(hypnogram$event), 
                      stringsAsFactors = FALSE)
  nodes$label <- nodes$group
  nodes$shape = "circle"
  nodes$event <- nodes$label
  nodes <- merge(unique(hypnogram[,c("event","count")]),nodes)
  nodes$event <- NULL
  nodes$value <- nodes$count
  
  # Edges
  edges <- hypnogram[hypnogram$nextstage != hypnogram$event,c("nextstage","event")]
  colnames(edges) <- c("to","from")
  edges$count = 1
  edges <- stats::aggregate(count ~ from + to, data = edges, sum)
  edges$group <- edges$to
  edges <- merge(edges,nodes[,c("group","id")], by = "group")
  edges$to <- edges$id
  edges$id <- NULL
  edges$group <- edges$from
  edges <- merge(edges,nodes[,c("group","id")], by = "group")
  edges$from <- edges$id
  edges$id <- NULL
  edges$group <- NULL
  edgestotal <- stats::aggregate(count ~ from, data = edges, sum)
  colnames(edgestotal) <- c("from","totalfrom")
  edges <- merge(edges,edgestotal,by = "from")
  edges$label <- round(edges$count/edges$totalfrom,digits=2)
  edges$width <- edges$label*4
  edges$arrows <- "to"
  edges$length <- 500
  visNetwork::visNetwork(nodes, edges, height = height, width = width)
  
}
