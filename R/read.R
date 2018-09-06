#' Read a MDF
#'
#' @param mdfPath MDF path.
#' @param channels Channels to read.
#' @return A (large) list containing signals and metadata.
read.mdf <- function(mdfPath, channels = c(NA)) {
  
  # Init list
  mdf <- list()
  
  # Filter channels to read
  mdfchannels <- list.dirs(mdfPath,full.names = FALSE)[-1]
  if (length(channels) > 0){
    if (!is.na(channels[1])){
      mdfchannels <- mdfchannels[mdfchannels %in% channels]
    }
  } else {
    mdfchannels <- c()
  }
  
  for (channel in mdfchannels){
    mdf[["channels"]][[channel]][["metadata"]] <- jsonlite::read_json(
      paste0(mdfPath,"/",channel,"/metadata.json"))
    mdf[["channels"]][[channel]][["signal"]] <- readBin(
      con = paste0(mdfPath,"/",channel,"/data.bin"),
      what = "numeric",
      endian = "little",
      n = mdf[["channels"]][[channel]][["metadata"]]$sLength,
      size = 4)
  }
  mdf[["metadata"]] <- jsonlite::read_json(paste0(mdfPath,"/metadata.json"))
  
  eventsPath <- paste0(mdfPath,"/events.json")
  if(file.exists(eventsPath)){
    mdf[["events"]] <- jsonlite::read_json(eventsPath,simplifyVector = TRUE)
    mdf[["events"]]$begin <- as.POSIXlt(mdf[["events"]]$begin)
    mdf[["events"]]$end <- as.POSIXlt(mdf[["events"]]$end)
  }
  
  return(mdf)
}
