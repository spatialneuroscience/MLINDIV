library(rprime)

get_sync_pulses <- function(subjectnum, eprime_directory, outdir = "D:/SampleData"){
  working_dir <- paste0(eprime_directory, "/", subjectnum)
  setwd(working_dir)
  filelist <- list.files()[grepl('.txt', list.files())]
  
  subject <- c()
  trial_type <- c()
  procedure <- c()
  getready_onset <- c()
  getready_response <- c()
  
  for (i in 1:length(filelist)){
    test <- read_eprime(filelist[i])
    
    subject <- c(subject, strsplit(filelist[i], "-")[[1]][2])
    procedure <- c(procedure, strsplit(strsplit(filelist[i], "-")[[1]][1], "_")[[1]][1])
    tt <- paste0(strsplit(filelist[i], "")[[1]][13], strsplit(filelist[i], "")[[1]][15])
    if (grepl('t', tt)){tt <- strsplit(tt, "")[[1]][1]}
    trial_type <- c(trial_type, tt)
    getready_onset <- c(getready_onset, strsplit(test[grepl("GetReady.OnsetTime", test)], " ")[[1]][2])
    getready_response <- c(getready_response, strsplit(test[grepl("GetReady.RTTime", test)], " ")[[1]][2])
  }
  
  sync_pulses <- cbind(subject, trial_type, procedure, getready_onset, getready_response)
  
  sync_pulses <- as.data.frame(sync_pulses)

  
  write.csv(sync_pulses, sprintf("%s/sub-%s_syncpulses.csv", outdir, subjectnum), row.names = FALSE)
    
}