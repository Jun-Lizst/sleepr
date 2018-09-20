# #http://sleeptight.isr.uc.pt/ISRUC_Sleep/
# download_isruc <- function(path){
#   #http://sleeptight.isr.uc.pt/ISRUC_Sleep/ISRUC_Sleep/subgroupI/1.rar
# }
# 
# #https://www.physionet.org/pn4/sleep-edfx/
# #https://physionet.org/physiobank/database/sleep-edfx/
# download_sleepedfx <- function(path){
#   
# }
# 
# #http://www.tcts.fpms.ac.be/~devuyst/Databases/DatabaseSubjects/
# download_dreams <- function(path){
#   
# }
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