#' Download isruc db
#'
#' @param target target path.
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

#' Download Sleepedfx db
#'
#' @param path target path.
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

# 
# #http://www.tcts.fpms.ac.be/~devuyst/Databases/DatabaseSubjects/
#http://www.tcts.fpms.ac.be/~devuyst/Databases/DatabaseSubjects/
#download_dreams_subjects <- function(path){

}
# 
# #https://physionet.org/physiobank/database/capslpdb/
# download_capsleep <- function(path){
#   
# }

#' Download the DREAMS Spindles database in the desired path. Check MD5 of the downloaded .rar file.
#' See the DREAMS Spindles database  website for more details: <http://www.tcts.fpms.ac.be/~devuyst/Databases/DatabaseSpindles/> 
#' @param path Path 
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