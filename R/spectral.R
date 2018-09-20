#' Compute bands powers.
#'
#' @param x signal vector.
#' @param Fs Frequency.
#' @param bands bands. Default "delta","theta","alpha","beta", "gamma1".
#' @param bands_names bands names
#' @param normalize normalize.
#' @return a named vector with all bands powers.
#' @export
bands_power <- function(x,
                        Fs,
                        bands=list(c(0.5,3.5),
                                   c(3.5,8),
                                   c(8,12),
                                   c(12,30),
                                   c(30,40)),
                       bands_names = c("delta",
                                       "theta",
                                       "alpha",
                                       "beta",
                                       "gamma1"),
                       normalize = c(10,90)
                       ){
  powers <- c()
  for(band in bands){
    power <- sum(eegkit::eegfft(x,Fs,band[1],band[2])$strength)
    if(length(normalize) == 2){
      power <- power/sum(eegkit::eegfft(x,Fs,normalize[1],normalize[2])$strength)
    }
    powers <- c(powers,power)
  }
  names(powers) <- bands_names
  return(powers)
}

#' Compute bands powers from a full hypnogram.
#'
#' @param record record.
#' @param channel channel to split and hyp.
#' @param bands bands. Default "delta","theta","alpha","beta", "gamma1", "denominator", "broadband".
#' @param bands_names band_names.
#' @param normalize normalize.
#' @return a df.
#' @export
hypnogram_band_powers <- function(record,
                                  channel,
                                  bands=list(c(0.5,3.5),c(3.5,8),
                                             c(8,12),c(12,30),
                                             c(30,40),
                                             c(4,40),c(0.5,40)),
                                  bands_names=c("delta","theta","alpha",
                                                "beta","gamma1",
                                                "denominator","broadband"),
                                  normalize = c(4,40)){
  sRate <- record[["channels"]][[channel]][["metadata"]][["sRate"]]
  hypnogram <- hypnogram(record[["events"]])
  signal <- sleepr::split_signal(signal = record[["channels"]][[channel]][["signal"]],
                                 hypnogram = hypnogram,
                                 sRate = sRate)
  pw <- plyr::ldply(lapply(signal,function(x){
    sleepr::bands_power(x = x, Fs = sRate, bands = bands,
                        bands_names = bands_names,
                        normalize = normalize)
  }))
  pw$stage <- hypnogram$event
  pw$epoch <- c(1:nrow(pw))
  return(pw)
}
