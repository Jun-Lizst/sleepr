#' Read ISRUC sleep scoring
#'
#' @param dir ISRUC record directory.
#' @param scoringNum Scoring number in database.
#' @return A dataframe of scored events.
read.events.isruc <- function(dir,scoringNum){
  xlsxPath <- list.files(dir,
                         pattern = paste0("_",scoringNum,".xlsx"),
                         full.names = TRUE)[1]
  recPath <- list.files(dir,
                        pattern = ".rec",
                        full.names = TRUE)[1]
  headers <- edfReader::readEdfHeader(recPath)
  start <- headers$startTime

  scoring <- readxl::read_xlsx(xlsxPath)
  scoring <- scoring[!is.na(scoring$Epoch),]
  scoring$begin <- as.numeric(start+(scoring$Epoch*30-30))
  scoring$end <- scoring$begin+30

  # Stages
  stages <- scoring[,c("Stage","begin","end")]
  colnames(stages)[colnames(stages)=="Stage"] <- "event"
  stages$event[stages$event == "R"] <- "REM"
  stages$event[stages$event == "W"] <- "AWA"
  
  stages$begin <- as.POSIXlt(stages$begin,origin= "1970-01-01 00:00.00 UTC")
  stages$end <-  as.POSIXlt(stages$end,origin= "1970-01-01 00:00.00 UTC")

  return(stages)
}

#' Read a Noxturnal events file (Unicode CSV format)
#'
#' @param path Noxturnal events file path.
#' @return A dataframe of scored events.
read.events.noxturnal <- function(path){

  events <- tryCatch({
    read.csv(path,
             fileEncoding = "UTF-8")
  }, error = function(e){
    read.csv(path,
             fileEncoding = "UTF-16")
  }, warning = function(e){
    read.csv(path,
             fileEncoding = "UTF-16")
  }
  )

  events <- events[,1:4]
  if(events[1,1][1] == "[]"){
    events <- events[-1,]
  }

  for (i in 1:4){
    if(colnames(events)[i] == "Heure.de.d.but" | colnames(events)[i] == "Heure.de.début"){
      colnames(events)[i] <- "begin"
      events$begin <- strptime(events$begin, format = "%d/%m/%Y %H:%M:%S")
    } else if(colnames(events)[i] == "Heure.de.fin") {
      colnames(events)[i] <- "end"
      events$end <- strptime(events$end, format = "%d/%m/%Y %H:%M:%S")
    } else if(colnames(events)[i] == "X.v.nement" | colnames(events)[i] == "Événement") {
      colnames(events)[i] <- "event"
      events$event <- as.character(events$event)
    } else if(colnames(events)[i] == "Dur.e" | colnames(events)[i] == "Durée") {
      colnames(events)[i] <- "duration"
      events$duration <- as.numeric(events$duration)
    }
  }

  events$duration <- NULL
  events$event[events$event == "?veil"] <- "Éveil"
  events$event[events$event == "Éveil"] <- "AWA"
  events$event[events$event == "D?but de l'analyse"] <- "Début de l'analyse"
  events$event[events$event == "Micro-?veil"] <- "Micro-Éveil"
  events$event[events$event == "Hypopn?e"] <- "Hypopnée"
  events$event[events$event == "D?sat"] <- "Désat"

  return(events)
}
