#' Compute bands powers.
#'
#' @param x signal vector.
#' @param Fs Frequency.
#' @param bands bands.
#' @param normalize normalize.
#' @return a named vector with all bands powers.
#' @export
bands_power <- function(x,Fs,bands,normalize){
  powers <- c()
  for(band_label in names(bands)){
    power <- sum(eegkit::eegfft(x,Fs,bands[[band_label]][1],bands[[band_label]][2])$strength)
    if(length(normalize) == 2){
      power <- power/sum(eegkit::eegfft(x,Fs,normalize[1],normalize[2])$strength)
    }
    powers <- c(powers,power)
  }
  names(powers) <- names(bands)
  return(powers)
}

#' Compute bands powers from a full hypnogram.
#'
#' @param record record.
#' @param channel channel to split and hyp.
#' @param bands bands. Default "delta","theta","alpha","beta", "gamma1", "denominator", "broadband".
#' @param normalize normalize.
#' @return a df.
#' @export
hypnogram_band_powers <- function(record,
                                  channel,
                                  bands=list(delta = c(0.5,3.5),
                                             theta = c(3.5,8),
                                             alpha = c(8,12),
                                             beta = c(12,30),
                                             gamma1 = c(30,40)),
                                  normalize = c(4,40)){
  sRate <- record[["channels"]][[channel]][["metadata"]][["sRate"]]
  hypnogram <- hypnogram(record[["events"]])
  signal <- sleepr::split_signal(signal = record[["channels"]][[channel]][["signal"]],
                                 hypnogram = hypnogram,
                                 sRate = sRate)
  pw <- plyr::ldply(lapply(signal,function(x){
    sleepr::bands_power(x = x, Fs = sRate, bands = bands, normalize = normalize)
  }))
  pw$stage <- hypnogram$event
  pw$epoch <- c(1:nrow(pw))
  return(pw)
}

#' aggregate_band_powers
#'
#' @param hypnogram_band_powers hypnogram_band_powers.
#' @return Wide dataframe. 
#' @export
aggregate_band_powers <- function(hypnogram_band_powers){
  colnames <- colnames(hypnogram_band_powers)[!(colnames(hypnogram_band_powers) %in% c("denominator","broadband","epoch"))]
  bandnames <- colnames[!(colnames %in% c("stage"))]
  means <- stats::aggregate(hypnogram_band_powers[,bandnames],by=list(hypnogram_band_powers$stage),FUN=mean,na.rm=TRUE)
  means$id <- 1
  means <- stats::reshape(means, idvar = "id", timevar = "Group.1", direction = "wide")
  means$id <- NULL
  colnames(means) <- tolower(gsub("\\.", "_", colnames(means)))
  return(means)
}