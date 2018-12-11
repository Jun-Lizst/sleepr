#' Compute bands powers.
#'
#' @param x signal vector.
#' @param Fs Frequency.
#' @param bands bands.
#' @param normalize normalize.
#' @param butter butter order or false.
#' @return a named vector with all bands powers.
#' @export
bands_power <- function(x,Fs,bands,normalize,butter = FALSE){
  #x[is.na(x)] <- 0
  if(butter != FALSE){
    filt <- signal::butter(butter, 0.1)
    x <- signal::filtfilt(filt, x)
  }
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
#' @param labels labels.
#' @param butter butter order
#' @param resampling sRate to resample by.
#' @return a df.
#' @export
hypnogram_band_powers <- function(record,
                                  channel,
                                  bands=list(delta = c(0.5,3.5),
                                             theta = c(3.5,8),
                                             alpha = c(8,12),
                                             beta = c(12,30),
                                             gamma1 = c(30,40)),
                                  normalize = c(4,40),
                                  labels = c("N3","N2","N1","REM","AWA"),
                                  butter = FALSE,
                                  resampling = FALSE){
  
  sRate <- record[["channels"]][[channel]][["metadata"]][["sRate"]]
  hypnogram <- hypnogram(record[["events"]],labels)
  signal <- record[["channels"]][[channel]][["signal"]]
  
  if(resampling != FALSE){
    signal <- signal::resample(signal, 1,(sRate/resampling) )
    sRate <- resampling
  }
  
  signal <- sleepr::split_signal(signal = signal,
                                 hypnogram = hypnogram,
                                 sRate = sRate)
  
  
  
  pw <- dplyr::bind_rows(lapply(signal,function(x){
    as.list(sleepr::bands_power(x = x, Fs = sRate, bands = bands, normalize = normalize, butter = butter))
  }))
  
  pw$stage <- hypnogram$event
  pw$epoch <- c(1:nrow(pw))
  return(pw)
}

#' Compute EEMD mean,var,skew,kur
#'
#' @param record record.
#' @param channel channel to split and hyp.
#' @param num_imfs numinfs
#' @param labels lablssl
#' @param butter butter
#' @return a df.
#' @export
hypnogram_eemd <- function(record, 
                           channel,
                           num_imfs = 8, 
                           labels = c("N3","N2","N1","REM","AWA"),
                           butter = FALSE){
  # Rlibeemd::eemd(c3a2.period$signal,
  #                num_imfs = 8)
  sRate <- record[["channels"]][[channel]][["metadata"]][["sRate"]]
  hypnogram <- hypnogram(record[["events"]],labels)
  signal <- sleepr::split_signal(signal = record[["channels"]][[channel]][["signal"]],
                                 hypnogram = hypnogram,
                                 sRate = sRate)
  eemddf <- data.frame()
  for(i in c(1:length(signal))){
    x <- signal[[i]]
    if(butter != FALSE){
      filt <- signal::butter(butter, 0.1)
      x <- signal::filtfilt(filt, x)
    }
    eemd <- Rlibeemd::eemd(x,num_imfs = num_imfs)
    epdf <- data.frame(epoch = i)
    for(j in c(1:num_imfs)){
      epdft <- data.frame(x1 = mean(eemd[,j]),
                          x2 = stats::var(eemd[,j]),
                          x3 = e1071::skewness(eemd[,j]),
                          x4 = e1071::kurtosis(eemd[,j]))
      colnames(epdft) <- c(paste0("eemd_",j,"_mean"),
                           paste0("eemd_",j,"_var"),
                           paste0("eemd_",j,"_skew"),
                           paste0("eemd_",j,"_kur"))
      epdf <- cbind(epdf,epdft)
    }
    eemddf <- dplyr::bind_rows(eemddf,epdf)
  }
  return(eemddf)
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
  hypnogram_band_powers_nrem <- hypnogram_band_powers[hypnogram_band_powers$stage %in% c("N1","N2","N3"),]
  if (nrow(hypnogram_band_powers_nrem) > 0){
    hypnogram_band_powers_nrem$stage <- "NREM"
    meansnrem <- stats::aggregate(hypnogram_band_powers_nrem[,bandnames],by=list(hypnogram_band_powers_nrem$stage),FUN=mean,na.rm=TRUE)
    means <- rbind(means,meansnrem)
  }
  means$id <- 1
  means <- stats::reshape(means, idvar = "id", timevar = "Group.1", direction = "wide")
  means$id <- NULL
  colnames(means) <- tolower(gsub("\\.", "_", colnames(means)))
  return(means)
}