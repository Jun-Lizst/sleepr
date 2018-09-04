#' Write a MDF from an EDF file. MDF directory is erased if it already exists.
#'
#' @param edfPath EDF file path.
#' @param mdfPath MDF path.
#' @param channels Channels to write.
write.mdf <- function(edfPath, mdfPath, channels = c()) {

  # Read EDF
  headers <- edfReader::readEdfHeader(edfPath)
  signals <- edfReader::readEdfSignals(headers)

  # Reset MDF directory
  if(dir.exists(mdfPath)){
    unlink(mdfPath, recursive = TRUE)
  } else {
    dir.create(mdfPath)
  }

  # Write metadata
  metadata <- headers
  metadata$sHeaders <- NULL
  metadata <- lapply(metadata, function(x) x[1])
  jsonlite::write_json(metadata,
                       path = paste0(mdfPath,
                                     "/metadata.json"),
                       auto_unbox = TRUE)

  # Write each channel
  edfchannels <- headers$sHeaders$label
  if (length(channels) > 0){
    edfchannels <- edfchannels[edfchannels %in% channels]
  }
  for(channel in edfchannels){

    signal <- signals[[channel]]

    if (!is.null(signal)){

      # Create channel directory
      channelPath <- paste0(mdfPath,"/",channel)
      dir.create(channelPath)

      # Write file
      writeBin(signal$signal,
               con = paste0(channelPath,"/data.bin"),
               endian = "little", size = 4)

      # Write metadata
      metadata <- headers$sHeaders[headers$sHeaders$label == channel,]
      jsonlite::write_json(as.list(metadata),
                           path = paste0(channelPath,"/metadata.json"),
                           auto_unbox = TRUE)
    } else {
      warning(
        paste0("Signal ",channel," corrupted.")
      )
    }
  }
}



