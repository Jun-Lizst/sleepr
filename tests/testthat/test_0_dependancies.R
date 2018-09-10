context("Checking dependancies")

# ISRUC file example ----
url <- "http://sleeptight.isr.uc.pt/ISRUC_Sleep/ISRUC_Sleep/subgroupI/1.rar"
file <- "data/1.rar"

if(!file.exists(paste0("data/1/1.rar"))){
  download.file(url,
                destfile = file)
  system(paste0("unrar x ",file, " data/"))
  file.copy(file,to = "data/1/")
  file.remove(file)
}

# Sleep EDF expanded example ----
file <- "data/ST7132J0-PSG.edf"
mdfPath <- "data/ST7132J0-PSG"

if(!file.exists(file)){
  download.file("https://physionet.org/pn4/sleep-edfx/ST7132J0-PSG.edf",
                destfile = file)
}