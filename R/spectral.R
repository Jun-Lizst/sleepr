#' Campute bands powers.
#'
#' @param x signal vector.
#' @param Fs Frequency.
#' @param bands bands. Default "delta","theta","alpha","beta", "gamma1".
#' @return a named vector with all bands powers.
bands.power <- function(x,Fs,bands=list(c(0.5,3.5),
                                       c(3.5,8),
                                       c(8,12),
                                       c(12,30),
                                       c(30,50)),
                       bands.names = c("delta","theta","alpha","beta", "gamma1")){
  powers <- c()
  for(band in bands){
    powers <- c(powers,sum(eegkit::eegfft(x,Fs,band[1],band[2])$strength))
  }
  names(powers) <- bands.names
  return(powers) 
}
