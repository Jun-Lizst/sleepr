#' Download the ISRUC-SLEEP Dataset in the desired path.
#' See the ISRUC-SLEEP Dataset website for more details: \url{https://sleeptight.isr.uc.pt/ISRUC_Sleep/}
#' @param target character, path to download database.
#' @references Sirvan Khalighi, Teresa Sousa, Jose Moutinho Santos, Urbano Nunes, ISRUC-Sleep: a comprehensive public dataset for sleep researchers, Computer Methods and Programs in Biomedicine, Elsevier, 2015
#' @export
download_isruc <- function(target){
  
  # Subgroups populations
  pops <- list(c(1:100),c(1:8),c(1:10))
  
  for (subgroup in c(1:3)){
    pop <- pops[[subgroup]]
    dir <- paste0(target,subgroup,"/")
    if(!dir.exists(dir)){
      dir.create(dir)
    }
    subchars = paste(rep("I",subgroup),
                     collapse = "")
    url <- paste0(
      "http://sleeptight.isr.uc.pt/",
      "ISRUC_Sleep/ISRUC_Sleep/subgroup",
      subchars,"/"
    )
    for (i in pop){
      filename <- paste0(i,".rar")
      filePath <- paste0(dir,filename)
      if (!file.exists(filePath)){
        furl <- paste0(url,i,".rar") 
        utils::download.file(url = furl,
                      destfile = filePath)
        system(paste0("unrar x ",filePath,
                      " ",dir))
      }
    }
  }
  
  for(i in c(1:3)){
    subchars = paste(rep("I",i),
                     collapse = "")
    path <- paste0(target,i,"/metadata.xlsx")
    url <- paste0("http://sleeptight.isr.uc.pt/",
                  "ISRUC_Sleep/ISRUC_Sleep/",
                  "Details/Details_subgroup_",
                  subchars,"_Submission.xlsx")
    if(!file.exists(path)){
      utils::download.file(url,destfile = path)
    }
  }
  
}

#' Download the Expanded Sleep-EDF Database
#' See the Expanded Sleep-EDF Database website for more details: \url{https://physionet.org/pn4/sleep-edfx/}
#' @param path character, path to download database.
#' @references B Kemp, AH Zwinderman, B Tuk, HAC Kamphuisen, JJL Oberyé. Analysis of a sleep-dependent neuronal feedback loop: the slow-wave microcontinuity of the EEG. IEEE-BME 47(9):1185-1194 (2000). 
#' @references Goldberger AL, Amaral LAN, Glass L, Hausdorff JM, Ivanov PCh, Mark RG, Mietus JE, Moody GB, Peng C-K, Stanley HE. PhysioBank, PhysioToolkit, and PhysioNet: Components of a New Research Resource for Complex Physiologic Signals. Circulation 101(23):e215-e220 [Circulation Electronic Pages; http://circ.ahajournals.org/cgi/content/full/101/23/e215]; 2000 (June 13). 
#' @export
download_sleepedfx <- function(path){
  if(!dir.exists(path)) dir.create(path)
  url <- "https://www.physionet.org/pn4/sleep-edfx/"
  if(!file.exists(paste0(path,"/SHA256SUMS"))) utils::download.file(paste0(url,"SHA256SUMS"), destfile = paste0(path,"/SHA256SUMS"))
  sha256 <- utils::read.table(paste0(path,"/SHA256SUMS"),header = FALSE,col.names = c("sha256","filename"),stringsAsFactors = F)
  for(i in c(1:nrow(sha256))){
    utils::download.file(url = paste0(url,sha256$filename[i]),
                  destfile = paste0(path,"/",sha256$filename[i])
      )
  }
}

#' Download DREAMS Subjects database in the desired path.
#' See the  website for more details: <http://www.tcts.fpms.ac.be/~devuyst/Databases/DatabaseSubjects/>
#' @param path character, path to download database.
#' @references S. Devuyst, T. Dutoit, P. Stenuit, M. Kerkhofs, E. Stanus, "Canceling ECG Artifacts in EEG using a Modified Independent Component Analysis Approach", EURASIP Journal on Advances in Signal Processing, Volume 2008, Article ID 747325, Accepted 31 July 2008.
#' @export
download_dreams_subjects <- function(path){
  if(!dir.exists(path)) dir.create(path)
  file_path <- paste0(path,"/DatabaseSubjects.rar")
  if(!file.exists(file_path)){
    utils::download.file(url = "http://www.tcts.fpms.ac.be/~devuyst/Databases/DatabaseSubjects/DatabaseSubjects.rar",
                         destfile = file_path)
  }
  system(paste0("unrar x ",file_path," ",path))
}

#' Download the DREAMS Spindles database in the desired path. Checks MD5 of the downloaded .rar file.
#' See the DREAMS Spindles database website for more details: \url{http://www.tcts.fpms.ac.be/~devuyst/Databases/DatabaseSpindles/} 
#' @param path character, path to download database. 
#' @references S. Devuyst et al., "Automatic Sleep Spindle Detection in Patients with Sleep Disorders," 2006 International Conference of the IEEE Engineering in Medicine and Biology Society, New York, NY, 2006, pp. 3883-3886. doi: 10.1109/IEMBS.2006.259298
#' @export
download_dreams_spindles <- function(path){
  if(!dir.exists(path)){
    dir.create(path)
  }
  rar_md5 <- "2f8a101194e133dd4324f21047dce579"
  rar_path <- paste0(path,"/DatabaseSpindles.rar")
  if(!file.exists(rar_path) & !(rar_md5 == tools::md5sum(rar_path))){
    utils::download.file(url = "http://www.tcts.fpms.ac.be/~devuyst/Databases/DatabaseSpindles/DatabaseSpindles.rar",
                  destfile = rar_path)
  }
  if(!file.exists(paste0(path,"excerpt2.edf"))){
    if(rar_md5 == tools::md5sum(rar_path)){
      system(paste0("unrar x ",rar_path," ",path))
    } else {
      stop("Invalid MD5")
    }
  }
}

#' Download the CAP Sleep Database in the desired path. 
#' See the CAP Sleep Database website for more details: \url{https://physionet.org/physiobank/database/capslpdb/}
#' @param path character, path to download database.
#' @references MG Terzano, L Parrino, A Sherieri, R Chervin, S Chokroverty, C Guilleminault, M Hirshkowitz, M Mahowald, H Moldofsky, A Rosa, R Thomas, A Walters. Atlas, rules, and recording techniques for the scoring of cyclic alternating pattern (CAP) in human sleep. Sleep Med 2001 Nov; 2(6):537-553. Goldberger AL, Amaral LAN, Glass L, Hausdorff JM, Ivanov PCh, Mark RG, Mietus JE, Moody GB, Peng C-K, Stanley HE. PhysioBank, PhysioToolkit, and PhysioNet: Components of a New Research Resource for Complex Physiologic Signals. Circulation 101(23):e215-e220 [Circulation Electronic Pages; http://circ.ahajournals.org/cgi/content/full/101/23/e215]; 2000 (June 13).  
#' @export
download_capslpdb <- function(path){
  if(!dir.exists(path)){
    dir.create(path)
  }
  url <- "https://physionet.org/physiobank/database/capslpdb/"
  if(!file.exists(paste0(path,"/SHA256SUMS"))) utils::download.file(paste0(url,"SHA256SUMS"), destfile = paste0(path,"/SHA256SUMS"))
  sha256 <- utils::read.table(paste0(path,"/SHA256SUMS"),header = FALSE,col.names = c("sha256","filename"),stringsAsFactors = F)
  for(i in c(1:nrow(sha256))){
    utils::download.file(url = paste0(url,sha256$filename[i]),
                         destfile = paste0(path,"/",sha256$filename[i])
    )
  }
}