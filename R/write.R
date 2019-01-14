#' Write a European Data Format (EDF) record file to disk using Morpheo Data Format (MDF) guidelines.
#' Target directory is erased if it already exists. Signals are stored in binary file, 
#' events and metadata in JavaScript Object Notation (JSON) files.
#'
#' @references P. Bouchequet, D. Jin, G. Solelhac, M. Chennaoui, D. Leger, "Morpheo Data Format (MDF), un nouveau format de données simple, robuste et performant pour stocker et analyser les enregistrements de sommeil", Médecine du Sommeil, vol. 15, n 1, p. 48‑49, march 2018.
#' @param edfPath character. European Data Format (EDF) file path.
#' @param mdfPath character. Morpheo Data Format (MDF) directory path.
#' @param channels character. Vector of channels labels to write.
#' @param events dataframe. Events dataframe to write. Events dataframe. Dataframe must contain \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.
#' @param endian character. Endianess. \code{"big"} or \code{"little"}. Defaults to \code{"little"}.
#' @export
write_mdf <- function(edfPath, mdfPath, channels = c(NA), events = c(), endian="little") {
  
  # Reset MDF directory
  if(dir.exists(mdfPath)){
    unlink(mdfPath, recursive = TRUE)
    dir.create(mdfPath)
  } else {
    dir.create(mdfPath)
  }
  
  for(edf in edfPath){
    # Read EDF
    headers <- edfReader::readEdfHeader(edf)
    
    if (!is.null(channels)){ 
      if (!is.na(channels)){
      signals <- edfReader::readEdfSignals(headers,signals = channels)
      } else {
        signals <- edfReader::readEdfSignals(headers)
        
      }
      
      } else {
      signals <- edfReader::readEdfSignals(headers)
      
    }
    
    # Write each channel
    edfchannels <- headers$sHeaders$label
    
    if (length(channels) > 0){
      if (!is.na(channels[1])){
        edfchannels <- edfchannels[edfchannels %in% channels]
      }
    } else {
      edfchannels <- c()
    }
    
    for(channel in edfchannels){
      write_channel(channel, signals, headers, mdfPath, endian = endian)
    }
  }

  # Write metadata
  metadata <- headers
  metadata$sHeaders <- NULL
  metadata <- lapply(metadata, function(x) x[1])
  jsonlite::write_json(metadata,
                       path = paste0(mdfPath,
                                     "/metadata.json"),
                       auto_unbox = TRUE)
  
  # Write events
  if(length(events > 0)){
    jsonlite::write_json(events,
                         path = paste0(mdfPath,
                                       "/events.json"))
  }
}

#' Write a timeserie to disk using Morpheo Data Format (MDF) guidelines.
#'
#' @references P. Bouchequet, D. Jin, G. Solelhac, M. Chennaoui, D. Leger, "Morpheo Data Format (MDF), un nouveau format de données simple, robuste et performant pour stocker et analyser les enregistrements de sommeil", Médecine du Sommeil, vol. 15, n 1, p. 48‑49, march 2018.
#' @param channel character. Channel name.
#' @param signals list. European Data Format (EDF) signals list.
#' @param headers list. European Data Format (EDF) file headers.
#' @param mdfPath Morpheo Data Format (MDF) directory path.
#' @param endian character. Endianess. \code{"big"} or \code{"little"}. Defaults to \code{"little"}.
write_channel <- function(channel, signals, headers, mdfPath, endian="little"){

  signal <- signals[[channel]]
  
  if (is.null(signal)){
    tryCatch({
      signal <- signals
      },
      error = function(e) {
        warning(
          paste0("Signal ",channel," corrupted.")
        )
      }
    )
   
  }
  
  if (!is.null(signal)){
    
    # Create channel directory
    channelPath <- paste0(mdfPath,"/",channel)
    dir.create(channelPath)
    
    # Write file
    writeBin(signal$signal,
             con = paste0(channelPath,"/data.bin"),
             endian = endian, size = 4)
    
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



