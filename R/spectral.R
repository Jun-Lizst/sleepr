#' Campute bands powers.
#'
#' @param x signal vector.
#' @param Fs Frequency.
#' @param bands bands. Default "delta","theta","alpha","beta", "gamma1".
#' @return a named vector with all bands powers.
bands_power <- function(x,
                        Fs,
                        bands=list(c(0.5,3.5),
                                   c(3.5,8),
                                   c(8,12),
                                   c(12,30),
                                   c(30,50),
                                   c(50,100)),
                       bands_names = c("delta",
                                       "theta",
                                       "alpha",
                                       "beta",
                                       "gamma1",
                                       "gamma2"),
                       normalize = c(0,99)
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

hypnogram_band_powers <- function(record,
                                  channel,
                                  bands=list(c(0.5,3.5),c(3.5,8),
                                             c(8,12),c(12,30),
                                             c(30,50),c(50,99),
                                             c(4,40),c(0.5,99)),
                                  bands_names=c("delta","theta","alpha",
                                                "beta","gamma1","gamma2",
                                                "denominator","broadband")){
  sRate <- record[["channels"]][[channel]][["metadata"]][["sRate"]]
  hypnogram <- sleepr::get.hypnogram(record[["events"]])
  signal <- sleepr::split_signal(signal = record[["channels"]][[channel]][["signal"]],
                                 hypnogram = hypnogram,
                                 sRate = sRate)
  pw <- plyr::ldply(lapply(signal,function(x){
    sleepr::bands_power(x = x,Fs = sRate,bands = bands,
                        bands_names = bands_names)
  }))
  pw$stage <- hypnogram$event
  pw$epoch <- c(1:nrow(pw))
  return(pw)
}
