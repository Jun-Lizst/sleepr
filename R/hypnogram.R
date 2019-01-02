#' Draw a hypnogram with ggplot2.
#'
#' @description A hypnogram represents the stages of sleep as a function of time. \code{plot_hypnogram()} plot a hypnogram using the \code{ggplot2} library from stages sleep in an event dataframe. \code{REM} stage is higlighted in red.
#' @references Silber MH, Ancoli-Israel S, Bonnet MH, Chokroverty S, Grigg-Damberger MM, et al. (2007). "The visual scoring of sleep in adults". Journal of Clinical Sleep Medicine. 3 (2): 121–31. PMID 17557422
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event}
#' @param labels Sleep stages labels. Defaults to \code{c("N3","N2","N1","REM","AWA")}.
#' @return a ggplot object.
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

#' Filter and reorder an events dataframe to keep only sleep stages related-events.
#'
#' @description Remove non-sleep stages events and reorder dataframe rows using the \code{begin} column.
#' @param events Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event}
#' @param labels Sleep stages labels. Defaults to \code{c("N3","N2","N1","REM","AWA")}.
#' @return hypnogram dataframe.
#' @export
hypnogram <- function(events, labels = c("N3","N2","N1","REM","AWA")){
  stages <- events[events$event %in% labels,]
  stages$event <- factor(stages$event, levels = labels)
  stages <- stages[order(stages$begin),]
  return(stages)
}

#' Split signal using hypnogram events.
#'
#' @param signal Vector containing signal.
#' @param hypnogram hypnogram df.
#' @param sRate Signal sample rate.
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

#' Split signal a signal vector into epochs.
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

#' Plot a stages transition graph using the \code{visNetwork} package.
#' @description \code{plot_transitions} uses the \code{visNetwork} package to plot a stages transition \code{htmlWidget} graph showing stages as nodes and probabilities on edges.
#' @param e Events dataframe containing stages. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param height height in pct or px.
#' @param width width.
#' @return A \code{visNetwork} \code{htmlWidget}.
#' @examples 
#' e <- data.frame(begin = as.POSIXlt(seq(from = 0, to = 30*10, by = 30),origin = "1970-01-01"))
#' e$end <- as.POSIXlt(seq(from = 30, to = 30*11, by = 30), origin = "1970-01-01")
#' e$event = c("AWA","N1","N2","N3","N3","REM","N2","REM","N2","REM","AWA")
#' plot_transitions(e, height = "500px", width = "500px")
#' @export
plot_transitions <- function(e, height = "500px", width = "100%"){
  
  # Check events dataframe
  check_events(e)
  
  # Hypnogram
  h <- hypnogram(e)
  h$event <- as.character(h$event)
  
  hc <- data.frame(table(h$event))
  colnames(hc) <- c("event","value")
  
  h <- merge(h,hc, by = "event")
  h <- h[order(h$begin),]
  
  # Nodes
  nodes <- data.frame(id = 1:length(unique(h$event)),
                      shape = "circle",
                      event = unique(h$event),
                      stringsAsFactors = FALSE)
  nodes <- merge(unique(h[,c("event","value")]),nodes)
  nodes$label <- paste0(nodes$event," (",nodes$value,")")
  nodes$group <- nodes$label
  nodes$value <- NULL
  
  # Edges
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

#' Computes sleep stages transition matrix from selected stages.
#' @description A sleep stages transition matrix contains the probabilities of next stage transition for a given stage.
#' @param e Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param l Stages labels. Defaults to \code{c("AWA", "N1", "N2", "N3", "REM")}
#' @return A matrix.
#' @examples
#' e <- data.frame(begin = as.POSIXlt(seq(from = 0, to = 30*10, by = 30),origin = "1970-01-01"))
#' e$end <- as.POSIXlt(seq(from = 30, to = 30*11, by = 30), origin = "1970-01-01")
#' e$event = c("AWA","N1","N2","N3","N3","REM","N2","REM","N2","REM","AWA")
#' tm(e)
#' @export
tm <- function(e, l = c("AWA", "N1", "N2", "N3", "REM")){
  
  # Check events dataframe
  check_events(e)
  
  # Hypnogram
  h <- hypnogram(e, labels = l)
  h$event <- as.character(h$event)
  
  hc <- data.frame(table(h$event))
  colnames(hc) <- c("event","count")
  
  h <- h[order(h$begin),]
  
  h$n <- c(h$event[-1],NA)
  h$c <- 1
  
  c <- stats::aggregate(c ~ event + n, data = h, FUN = sum)
  
  c <- merge(c, hc, by = "event")
  
  c$p <- c$c/c$count
  c <- c[,colnames(c)[!(colnames(c) %in% c("c", "count"))]]
  
  c <- stats::reshape(c, idvar = "event", timevar = "n", direction = "wide")
  names(c) <- gsub("p.", "", names(c))
  c[is.na(c)]  <- 0
  row.names(c) <- c$event
  c$event <- NULL
  
  c <- c[order(row.names(c)),colnames(c)[order(colnames(c))]]
  
  c <- as.matrix(c)  
  
  c
  
}
