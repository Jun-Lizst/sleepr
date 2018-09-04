#' Read a MDF
#'
#' @param mdfPath MDF path.
#' @param channels Channels to read.
#' @return A (large) list containing signals and metadata.
read.mdf <- function(mdfPath, channels = c()) {
  mdf <- list()
  mdfchannels <- list.dirs(mdfPath,full.names = FALSE)[-1]
  if (length(channels) > 0){
    mdfchannels <- mdfchannels[mdfchannels %in% channels]
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
  return(mdf)
}
