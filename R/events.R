#' Read ISRUC sleep scoring
#'
#' @param dir ISRUC record directory.
#' @param scoringNum Scoring number in database.
#' @return A dataframe of scored events.
#' @export
read_events_isruc <- function(dir,scoringNum){
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
#' @export
read_events_noxturnal <- function(path){

  events <- tryCatch({
    utils::read.csv(path,
             fileEncoding = "UTF-8")
  }, error = function(e){
    utils::read.csv(path,
             fileEncoding = "UTF-16")
  }, warning = function(e){
    utils::read.csv(path,
             fileEncoding = "UTF-16")
  }
  )

  events <- events[,1:4]
  if(events[1,1][1] == "[]"){
    events <- events[-1,]
  }

  for (i in 1:4){
    if(colnames(events)[i] == "Heure.de.d.but" | colnames(events)[i] == paste0("Heure.de.d","\u00E9","but")){
      colnames(events)[i] <- "begin"
      events$begin <- strptime(events$begin, format = "%d/%m/%Y %H:%M:%S")
    } else if(colnames(events)[i] == "Heure.de.fin") {
      colnames(events)[i] <- "end"
      events$end <- strptime(events$end, format = "%d/%m/%Y %H:%M:%S")
    } else if(colnames(events)[i] == "X.v.nement" | colnames(events)[i] == paste0("\u00C9","v","\u00E9","nement")) {
      colnames(events)[i] <- "event"
      events$event <- as.character(events$event)
    } else if(colnames(events)[i] == "Dur.e" | colnames(events)[i] == paste0("Dur","\u00E9","e")) {
      colnames(events)[i] <- "duration"
      events$duration <- as.numeric(events$duration)
    }
  }

  events$duration <- NULL
  events$event[events$event == "?veil"] <- paste0("\u00C9","veil")
  events$event[events$event == paste0("\u00C9","veil")] <- "AWA"
  events$event[events$event == "D?but de l'analyse"] <- paste0("D","\u00E9","but de l'analyse")
  events$event[events$event == "Micro-?veil"] <- paste0("Micro-","\u00C9","veil")
  events$event[events$event == "Hypopn?e"] <- paste0("Hypopn","\u00E9","e")
  events$event[events$event == "D?sat"] <- paste0("D","\u00E9","sat")
  
  if(nrow(events[events$event == paste0("D","\u00E9","but de l'analyse"),]) > 0){
    events <- events[events$begin >= min(events$begin[events$event == paste0("D","\u00E9","but de l'analyse")]),]
  }
  
  # Normalize events names
  events$event[events$event == paste0("Micro-","\u00C9","veil")] <- "micro-arousal"
  events$event[events$event == paste0("Micro-","\u00C9","veil")] <- "micro-arousal"
  events$event[events$event == "Dos"] <- "back"
  events$event[events$event == "Gauche"] <- "left"
  events$event[events$event == "Droite"] <- "right"
  events$event[events$event == "Ventre"] <- "stomach"
  
  # Normalize Cycles
  events <- rbind(events,normalize_cycles(events))

  return(events)
}

#' Read a SleepEDFX events file EDF+
#'
#' @param path EDF+ path
#' @param update merge N3 and N4 or not
#' @return A dataframe of scored events.
#' @export
read_events_sleepedfx <- function(path, update = TRUE){
  
  h <- edfReader::readEdfHeader(path)
  s <- edfReader::readEdfSignals(h)
  events <- s[["annotations"]]
  events$begin <- events$onset + as.numeric(s[["startTime"]]) 
  events$end <- events$end + as.numeric(s[["startTime"]])
  events$event[events$annotation == "Sleep stage W"] <- "AWA"
  events$event[events$annotation == "Sleep stage 1"] <- "N1"
  events$event[events$annotation == "Sleep stage 2"] <- "N2"
  events$event[events$annotation == "Sleep stage 3"] <- "N3"
  events$event[events$annotation == "Sleep stage 4"] <- "N4"
  events$event[events$annotation == "Sleep stage R"] <- "REM"
  events <- events[,c("begin","end","event")]
  events_final <- utils::head(events,0)
  events <- events[order(events$begin),]
  events$duration <- events$end - events$begin
  events$epochs <- events$duration/30
  
  begin <- min(events$begin)
  for(i in c(1:nrow(events))){
    for(j in c(1:events[i,]$epochs)){
      end <- begin + 30
      events_final[nrow(events_final)+1,] <- list(begin,end,events[i,]$event)
      begin <- begin + 30
    }
  }
  
  if(update){
    events_final$event[events_final$annotation == "N4"] <- "N3"
  }
  
  events_final$begin <- as.POSIXlt(events_final$begin,origin= "1970-01-01 00:00.00 UTC")
  events_final$end <-  as.POSIXlt(events_final$end,origin= "1970-01-01 00:00.00 UTC")
  
  return(events_final)
}

normalize_cycles <- function(events){
  cycles_labels <- list(
    c("Activity-CLASSICstart","Activity-CLASSICend"),
    c("Activity-BEGINstart","Activity-BEGINend"),
    c("Activity-REMstart","Activity-REMend"),
    c("Activity-ENstart","Activity-ENend")
  )
  cycles_raw <- events[events$event %in% unlist(cycles_labels),]
  cycles <- events[0,]
  if(nrow(cycles_raw) >= 2){
    cycles_raw <- cycles_raw[order(cycles_raw$begin),]
    cycles_raw$used <- FALSE
    for(cycles_pairs in cycles_labels){
      for(i in c(1:nrow(cycles_raw))){
        if(cycles_raw$event[i] == cycles_pairs[1]){
          ends <- cycles_raw[cycles_raw$used == FALSE & cycles_raw$begin > cycles_raw$begin[i] & cycles_raw$event == cycles_pairs[2],]
          end <- ends[ends$begin == min(ends$begin),]
          begin <- cycles_raw[i,]
          if(nrow(begin) == 1 & nrow(end) == 1){
            event <- paste0("cycle-",gsub("Activity-","",gsub("start","",begin$event)))
            begin <- begin$begin
            end <- end$end
            cycle_df <- data.frame("begin" = begin,"end" = end,"event" = as.character(event))
            cycle_df$event <- as.character(event)
            cycles <- rbind(cycles,cycle_df)
          }
        }
      }
    }
  }
  return(cycles)
}