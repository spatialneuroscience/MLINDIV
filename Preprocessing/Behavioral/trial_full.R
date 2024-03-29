# 2
# Original Author: Robert Woodry
# FINAL Version Adapted By: Alina Tu
# Contact: alinat2@uci.edu
# Last Updated: 1/11/2023

# About: Split full file into Explore and Test
#        Split test full file by rows where movement = "Select"
#        Split Explore files by NAs
#        For all these newly split data frames grab the letter_locs and store them in as path vectors in 
#              lists that will serve as individual observations in an eventual new file for 
#              trial by trial summary
# Changes in FINAL Version: New working_dir path, "master" -> "full," "EndAt" -> "end_location"
#        because "EndAt" is the target object and only matches participants' end locations if trial is correct,
#   [12/13/22 changes]  od and id values were initially switched - now fixed, 
#   [1/4/23 changes]    getready_rttime, iti_onset, startat_onset, goal_onset, and session_time variables added,
#   [1/8/23 changes]    distancetable.csv file path updated because the previous distancetable.csv was incomplete,
#   [1/11/23 changes]   changed CSV output directory to ...data/MLINDIV/preprocessed/behavioral instead of ...data/MLINDIV/raw/raw_behav,
#                       test trials scored as "accurate" where a) participants were supposed to go to N but ended at F facing south and b) participants were supposed to go to Y but ended at V facing east
# Rob's original script is now in /mnt/chrastil/lab/users/rob/scripts/MLINDIV/OldVersions/
# Rob's output data is now in /mnt/chrastil/lab/data/MLINDIV/raw/raw_behav/OldVersions/

# set working directory here to where the full csv file is located along with the location and dista
# setwd("/mnt/chrastil/lab/users/alina") # use personal working directory (instead of the row below) to test changes
setwd("/mnt/chrastil/lab/data/MLINDIV/preprocessed/behavioral")


full_file <- read.csv("MLINDIV_behavioral_full.csv")

library(tidyverse)
library(plyr)

tt_rows <- which(c(FALSE, tail(full_file$Task_type,-1) != head(full_file$Task_type,-1)))
s_rows <- which(c(FALSE, tail(full_file$Sample,-1) != head(full_file$Sample,-1)))
splitrows <- sort(c(tt_rows, s_rows))
splitrows <- splitrows - 1
meta <- split(full_file, cumsum(1:nrow(full_file) %in% (splitrows+1)))

# Meta contains all the different trials within each procedure ran for each participant
paths <- c()
eprocs <- c()
Subject <- c()
Task <- c()
Task_type <- c()
Procedure <- c()
Sample <- c()
Session_time <- c()
GetReady.RTTime <- c()
Finished.OnsetTime <- c()
ITI.OnsetTime <- c()
StartAt.OnsetTime <- c()
Goal.OnsetTime <- c()
objlist <- c()
PairList <- c()
startPosition <- c()
startFacing <- c()
EndIm <- c()
StartIm <- c()
Vid_paths <- c()
select_location <- c()
end_rotation <- c()
accuracy <- c()
nodesturns_count <- c()
nodes_count <- c()
average_RT <- c()
trial_starttime <- c()
trial_endtime <- c()
trial_duration <- c()
turns_count <- c()
select_made <- c()
od <- c()
id <- c()
le <- c()
final_movevid_OT <- c()
mvtrial_duration <- c()
e_paths <- c()

print("before Loop")

for (i in 1:length(meta)){
  print(i)
  #Path Hallsnip Vectors
  pvid <- meta[[i]]["hallsnip"][[1]]
  last_vp <- tail(pvid, 1)
  pvid <- paste0(pvid[!is.na(pvid)], collapse=" ")
  Vid_paths <- c(Vid_paths, pvid)
  
  # Selection Made
  select_made <- c(select_made, last_vp == "Selec")
  
  # Path vectors
  path <- meta[[i]]["letter_loc"][[1]]
  path <- paste0(path[!is.na(path)], collapse=" ")
  path <- paste(substr(pvid, 1, 1), path)
  paths <- c(paths, path)
  
  # Expanded path vectors
  e_path <- substr(meta[[i]]["end_location"][[1]], 1, 2)
  e_path <- paste0(e_path[!is.na(path)], collapse=" ")
  e_paths <- c(e_paths, e_path)
  
  # Eprime Procedure file name
  eproc_name <- as.character(meta[[i]]["Eprime.Basename"][[1]][1])
  eprocs <- c(eprocs, eproc_name)
  
  # Subject number
  subject <- as.character(meta[[i]]["Subject"][[1]][1])
  Subject <- c(Subject, subject)
  
  # Task
  task <- as.character(meta[[i]]["Task"][[1]][1])
  Task <- c(Task, task)
  
  # Task_type
  task_type <- as.character(meta[[i]]["Task_type"][[1]][1])
  Task_type <- c(Task_type, task_type)
  
  # Procedure
  proc <- as.character(meta[[i]]["Procedure"][[1]][1])
  Procedure <- c(Procedure, proc)
  
  # Sample
  sample <- as.character(meta[[i]]["Sample"][[1]][1])
  Sample <- c(Sample, sample)
  
  # Session Time
  session_time <- as.character(meta[[i]]["Session_time"][[1]][1])
  Session_time <- c(Session_time, session_time)
  
  # GetReady.RTTime
  getready_rtt <- as.character(meta[[i]]["GetReady.RTTime"][[1]][1])
  GetReady.RTTime <- c(GetReady.RTTime, getready_rtt)
  
  # Finished.OnsetTime
  finish_onset <- as.character(meta[[i]]["Finished.OnsetTime"][[1]][1])
  Finished.OnsetTime <- c(Finished.OnsetTime, finish_onset)
  
  # ITI.OnsetTime
  iti_onset <- as.character(meta[[i]]["ITI.OnsetTime"][[1]][1])
  ITI.OnsetTime <- c(ITI.OnsetTime, iti_onset)
  
  # StartAt.OnsetTime
  startim_onset <- as.character(meta[[i]]["StartAt.OnsetTime"][[1]][1])
  StartAt.OnsetTime <- c(StartAt.OnsetTime, startim_onset)
  
  # Goal.OnsetTime
  goal_onset <- as.character(meta[[i]]["Goal.OnsetTime"][[1]][1])
  Goal.OnsetTime <- c(Goal.OnsetTime, goal_onset)
  
  # objlist
  obj <- as.character(meta[[i]]["objlist"][[1]][1])
  objlist <- c(objlist, obj)
  
  # pairlist
  pair <- as.character(meta[[i]]["pairlist"][[1]][1])
  PairList <- c(PairList, pair)
  
  # startPosition
  sp <- as.character(meta[[i]]["startPosition"][[1]][1])
  startPosition <- c(startPosition, sp)
  
  # startFacing
  sf <- as.character(meta[[i]]["startFacing"][[1]][1])
  startFacing <- c(startFacing, sf)
  
  # StartIm
  si <- substr(as.character(meta[[i]]["StartIm"][[1]][1]), 8, 8)
  if (si == "V" & !is.na(si)){ si <- "Y"}
  StartIm <- c(StartIm, si)
  
  # EndIm
  ei <- substr(as.character(meta[[i]]["EndIm"][[1]][1]), 8, 8)
  if(ei == "V" & !is.na(ei)){ei <- "Y"}
  EndIm <- c(EndIm, ei)
  
  #ObjDur
  od <- c(od, as.numeric(as.character((meta[[i]]["ObjDur"][[1]][1]))))
  
  #ItiDur
  id <- c(id, as.numeric(as.character((meta[[i]]["ITIDur"][[1]][1]))))
  
  # end_location
  nc <- nchar(path)
  last <- substr(path, nc, nc)
  select_location <- c(select_location, last)
  
  # end_rotation
  er <- meta[[i]]["face_dir"][[1]]
  er <- tail(er[!is.na(er)], 1)
  end_rotation <- c(end_rotation, er)
  
  # v-----Custom Calculations-----v
  
  # Accuracy
  ac <- last == ei
  accuracy <- c(accuracy, ac)
  
  # Nodes Turns Count
  nodes <- strsplit(path, " ")[[1]]
  ncount <- length(nodes)
  nodesturns_count <- c(nodesturns_count, ncount)
  
  # Nodes Count (no turns)
  vp <- strsplit(pvid, " ")[[1]]
  vp <- length(vp[substr(vp, 1, 1) != substr(vp, 4, 4)])
  nodes_count <- c(nodes_count, vp)
  
  # Turn Count
  tc <- ncount- vp
  turns_count <- c(turns_count, tc)

  # Average Response Time
  avRT <- mean(as.numeric(meta[[i]]["Choose.RT"][[1]]), na.rm=TRUE)
  average_RT <- c(average_RT, avRT)
  
  # Trial Completion Duration
  starttime <- as.numeric(as.character(meta[[i]]["Choose.OnsetTime"][[1]][1]))
  if (is.na(proc)){
    endtime <- as.numeric(as.character(meta[[i]]["Finished.OnsetTime"][[1]][1]))
  } else {
    endtime <- as.numeric(as.character(meta[[i]]["ITI.OnsetTime"][[1]][1]))
  }
 
  # endtime <- as.numeric(as.character(meta[[i]]["Choose.RTTime"][[1]]))
  # if (length(endtime) == 1 & is.na(tail(endtime, 1))){
  #   endtime <- NA
  #   tctime <- NA
  # } else {
  #   endtime <- tail(endtime[!is.na(endtime)], 1)
  #   tctime <- endtime - starttime
  # }
  tctime <- endtime - starttime
  
  if (last_vp == "Selec" & !is.na(last_vp)){ tctime <- tctime - 1000} # Compansating for duration of Selection made vid duration
  trial_duration <- c(trial_duration, tctime)
  
  
  # Trial Start Time
  trial_starttime <- c(trial_starttime, starttime)
  
  # Trial Completion Time
  trial_endtime <- c(trial_endtime, endtime)
  
  # Final MoveVid Onset Time
  fmvot <- as.integer(as.character(tail(meta[[i]]["MoveVid.OnsetTime"][[1]], 1)))
  final_movevid_OT <- c(final_movevid_OT, fmvot)

}

trial_full <- data.frame(Subject = Subject, eprocs = eprocs, Task_type = Task_type, Procedure = Procedure,
                           Sample = Sample, objlist = objlist, PairList = PairList, ITIDur = id, ObjDur = od, startPosition = startPosition, 
                           startFacing = startFacing, paths = paths, e_paths = e_paths, Vid_paths = Vid_paths, select_made = select_made, StartAt = StartIm, EndAt = EndIm, 
                           end_location = select_location, end_rotation = end_rotation, accuracy = accuracy, 
                           nodesturns_count = nodesturns_count, nodes_count = nodes_count, turns_count = turns_count, average_RT = average_RT,
                           final_movevid_OT = final_movevid_OT, trial_starttime = trial_starttime, trial_endtime = trial_endtime, trial_duration = trial_duration,
                           getready_rttime = GetReady.RTTime, iti_onset = ITI.OnsetTime, startat_onset = StartAt.OnsetTime, goal_onset = Goal.OnsetTime, session_time = Session_time
                           )


working_dir <- "/mnt/chrastil/lab/data/MLINDIV/raw/raw_behav"
setwd(working_dir)

dtable <- read.csv("distancetable.csv")
dtable[30, 1] <- "P"
dtable[44, 1] <- "P"
colnames(dtable)[1] <- "StartAt"
colnames(dtable)[2] <- "end_location"

t <- join(trial_full, dtable, by = c("StartAt", "end_location"))

t <- t %>% mutate(fmv_duration = trial_endtime - final_movevid_OT)

#### 

# Recoding wayfinding trials that were supposed to end at N but actually ended at F facing south as true, as they are close enough to being true. 
# Likewise, recoding trials that were supposed to end at Y but actually ended at V facing east as true. 
recode_trials_1 <- which(t$Procedure == 'TrialProc' & t$EndAt == 'N' & t$end_location =='F' & t$end_rotation =='S') # finding rows satisfying first condition 
recode_trials_2 <- which(t$Procedure == 'TrialProc' & t$EndAt == 'Y' & t$end_location =='V' & t$end_rotation =='E') # finding rows satisfying second condition
recode_trials <- append(recode_trials_1, recode_trials_2) # combining into one list
t[recode_trials,16] <- "TRUE" # recoding these trials to TRUE 

# write.csv(t, file.path("/mnt/chrastil/lab/users/alina", "MLINDIV_trial_full.csv")) # use personal working directory (instead of the row below) to test changes
write.csv(t, file.path("/mnt/chrastil/lab/data/MLINDIV/preprocessed/behavioral", "MLINDIV_trial_full.csv"))
