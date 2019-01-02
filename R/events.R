#' Read scoring from the ISRUC-Sleep datasets.
#' 
#' @description Read a XLSX sleep stages scoring from  a ISRUC-Sleep dataset record directory.
#' @param dir character, ISRUC record directory.
#' @param scoringNum Scoring number in database.
#' @references Sirvan Khalighi, Teresa Sousa, Jose Moutinho Santos, Urbano Nunes, ISRUC-Sleep: a comprehensive public dataset for sleep researchers, Computer Methods and Programs in Biomedicine, Elsevier, 2015
#' @return A dataframe of scored stages.
#' @export
read_events_isruc <- function(dir, scoringNum = 1) 
{
  xlsxPath <- list.files(dir, pattern = paste0("_", scoringNum, 
                                               ".xlsx"), full.names = TRUE)[1]
  recPath <- list.files(dir, pattern = ".rec", full.names = TRUE)[1]
  
  if(is.na(recPath)){
    start <- as.POSIXlt("1970-01-01 00:00.00 UTC")
  } 
  # nocov start
  else { 
    start <- edfReader::readEdfHeader(recPath)$startTime }
  # nocov end
  
  scoring <- readxl::read_xlsx(xlsxPath, col_names = FALSE)[1:2]
  if(scoring$X__1[1] == "Epoch"){
    scoring <- scoring[-1,]
  }
  colnames(scoring) <- c("Epoch","Stage")
  scoring$Epoch <- as.numeric(scoring$Epoch)
  scoring <- scoring[!is.na(scoring$Epoch), ]
  scoring$begin <- as.numeric(start + (scoring$Epoch * 30 - 30))
  scoring$end <- scoring$begin + 30
  stages <- scoring[, c("Stage", "begin", "end")]
  colnames(stages)[colnames(stages) == "Stage"] <- "event"
  stages$event[stages$event == "R"] <- "REM"
  stages$event[stages$event == "W"] <- "AWA"
  stages$begin <- as.POSIXct(stages$begin, origin = "1970-01-01 00:00.00 UTC")
  stages$end <- as.POSIXct(stages$end, origin = "1970-01-01 00:00.00 UTC")
  return(stages)
}

#' Read ISRUC-Sleep dataset records metadata.
#'
#' @description Read all the subgroups XLSX metadata files and concatenates the dataframes into one.
#' @param dir ISRUC dataset directory.
#' @return A dataframe containing records metadata.
#' @export
read_isruc_metadata <- function(dir){
  m1 <- readxl::read_xlsx(paste0(dir,"1/metadata.xlsx"), skip=2) 
  m1$subgroup = 1
  m1$Subject <- as.character(m1$Subject)
  m2 <- readxl::read_xlsx(paste0(dir,"2/metadata.xlsx"),skip=2)
  m2$subgroup = 2
  m2$Age <- as.character(m2$Age)
  m2 <- m2[!is.na(m2$Subject),]
  m3 <- readxl::read_xlsx(paste0(dir,"3/metadata.xlsx"), skip=2)
  m3$subgroup = 3
  m3$Subject <- as.character(m3$Subject)
  m3$Age <- as.character(m3$Age)
  metadata <- dplyr::bind_rows(m1,m2,m3)
}

#' Read scoring from the ISRUC-Sleep datasets.
#'
#' @param dir ISRUC target.
#' @return A dataframe of scored events.
#' @export
read_all_events_isruc <- function(dir){
  hypnograms <- data.frame()
  for(subgroup in list.dirs(dir, T, F)){
    for(record in list.dirs(subgroup, T, F)){
      if(length(list.dirs(record, T, F)) > 1){
        record <- paste0(record,"/1")
      }
      hypnogram <- read_events_isruc(record, 1)
      hypnogram$record <- record
      hypnograms <- dplyr::bind_rows(hypnograms,
                                     hypnogram)
    }
  }
  return(hypnograms)
}


#' Read a Noxturnal events file (Unicode CSV format)
#'
#' @param dir Noxturnal events file path.
#' @return A dataframe of scored events.
#' @export
read_events_noxturnal <- function(dir){

  events <- tryCatch({
    utils::read.csv(dir,
             fileEncoding = "UTF-8")
  }, error = function(e){ utils::read.csv(dir, fileEncoding = "UTF-16") },
  warning = function(e){
    utils::read.csv(dir,
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
#' @param dir EDF+ path
#' @param update merge N3 and N4 or not
#' @return A dataframe of scored events.
#' @export
read_events_sleepedfx <- function(dir, update = TRUE){
  
  h <- edfReader::readEdfHeader(dir)
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
    events_final$event[events_final$event == "N4"] <- "N3"
  }
  
  events_final$begin <- as.POSIXlt(events_final$begin,origin= "1970-01-01 00:00.00 UTC")
  events_final$end <-  as.POSIXlt(events_final$end,origin= "1970-01-01 00:00.00 UTC")
  
  return(stats::na.omit(events_final))
}

#' normalize_cycles
#'
#' @param events events
#' @export
normalize_cycles <- function(events){
  cycles_labels <- list(
    c("Activity-CLASSICstart","Activity-CLASSICend"),
    c("Activity-BNstart","Activity-BNend"),
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

#' Read Subjects DB AASM scoring
#'
#' @param record_id record_id
#' @param path path.
#' @return A dataframe of scored events.
#' @export
read_events_dreams_subjects <- function(record_id, path){
  events_path <- paste0(path,"/","HypnogramAASM_",record_id,".txt")
  edf_path <- paste0(path,"/",record_id,".edf")
  startTime <- edfReader::readEdfHeader(edf_path)$startTime[1]
  events <- utils::read.table(events_path,skip = 1,col.names = "event")
  events$event[events$event == 5] <- "AWA"
  events$event[events$event == 4] <- "REM"
  events$event[events$event == 3] <- "N1"
  events$event[events$event == 2] <- "N2"
  events$event[events$event == 1] <- "N3"
  events$event[events$event == 0] <- "Unknown"
  events$event[events$event == -1] <- "Unknown"
  events$event[events$event == -2] <- "Unknown"
  events$event[events$event == -3] <- "Unknown"
  events$begin <- startTime + (c(0:(nrow(events)-1))*5)
  events$end <- events$begin + 5
  return(events)
}