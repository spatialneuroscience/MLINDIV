# 1 
# Original Author: Robert Woodry
# FINAL Version Adapted By: Alina Tu
# Contact: alinat2@uci.edu
# Last Updated: 1/4/2023

# About: This script goes through all participant file folders, compiles relevant Eprime behavioral data into a 
#         "tidy data" format, and appends it to a MLINDIV_behavioral_full.csv for further analysis
# Changes in FINAL Version: New working_dir path, as.tibble -> as_tibble, select -> dpylr::select, "master" -> "full"
#         to avoid unnecessary references to slavery (check out Github's switch from "master" to "main" branch in 2020),
#         fix errors in output to correct subject numbers for 052 C4, 076 B1, 113 2,
#   [1/4/23 changes]    GetReady_RTTime, StartAt.OnsetTime, Goal.OnsetTime, and Session_time variables added, extra column removed
# Rob's original script is now in /mnt/chrastil/lab/users/rob/scripts/MLINDIV/OldVersions/
# Rob's output data is now in /mnt/chrastil/lab/data/MLINDIV/raw/raw_behav/OldVersions/

# Load proper packages. Rprime is built specifically for EPrime. If you do not have these packages installed, use
#     install.packages("[package name goes here]") to install it on your R client.
library(rprime)
library(tidyverse)
library(wrapr)
library(plyr)

# Set your working directory here. the one below is mine. Copy and paste yours, and assign it to 'working_dir'. 
# This working directory should contain file folders for each participant, with each folder containing Eprime trial data.
# Each folder is named with participant's number:
# i.e.: "002.....103"
working_dir <- "/mnt/chrastil/lab/data/MLINDIV/raw/raw_behav"
setwd(working_dir)

# Create a list of file folders containing participant data
MLINDIV_filelist <- list.files()
MLINDIV_filelist <- MLINDIV_filelist[nchar(MLINDIV_filelist) == 3]

# Create an empty full data frame; this will contain all Explore and Test data for each participant.

full_file <- tibble()

# Loop through each participant's folder
for (participant_file_folder in 1:length(MLINDIV_filelist)){
  current_file <- MLINDIV_filelist[participant_file_folder]

  full_participant <- tibble()
  eprime_txt_files <- list.files(MLINDIV_filelist[participant_file_folder], pattern = ".*.txt")
  
  # Loop through each .txt file in that participant's folder, build a full participant file, to be appended
  # to the full file at the end of loop
  for (file_i in 1:length(eprime_txt_files)){
    
    
    eprime_txt_files <- list.files(MLINDIV_filelist[participant_file_folder], pattern = ".*.txt")
    
    # Create data frame, remove header/ender rows and store for later use
    e_file <- read_eprime(paste0(MLINDIV_filelist[participant_file_folder], "/" , eprime_txt_files[file_i]))
    
    # Check to see if file is complete
    if (last(e_file) != "*** LogFrame End ***"){
      print(paste(eprime_txt_files[file_i], "skipped", "incomplete file"))
      next
    }
    e_frame <- FrameList(e_file)
    e_df <- to_data_frame(e_frame)
    
    
    # Check to see if file contains minimum amount of rows for compilation
    if (nrow(e_df) < 10){
      print(paste(eprime_txt_files[file_i], "skipped", "row #", nrow(e_df)))
      next
    }
    e_df_header <- e_df[1, ]
   
    e_df <- e_df[2:(nrow(e_df)-1), ]

    
    eprime_test_txt_files <- list.files(MLINDIV_filelist[participant_file_folder], pattern = "Test.*.txt")
    print(eprime_txt_files[file_i])
    
    
    
    
    # If a test trial, create a separate tibble for the TrialProcedures, which contain start/end goals and other variables
    if (eprime_txt_files[file_i] %in% eprime_test_txt_files){
      # Create the trial_proc tibble containing the trial procedure meta-data
      trial_proc <- e_df[e_df$Procedure == "TrialProc" | e_df$Procedure == "TrialRevProc", ]
      trial_proc <- as_tibble(trial_proc) %>%
        dplyr::select(qc(
          Procedure, Sample, itilist, ITI.OnsetTime, 
          ITIDur, objlist, ObjDur, 
          pairlist, startPosition, startFacing, 
          StartIm, endPosition, EndIm,
          StartAt.OnsetTime, Goal.OnsetTime
        ))
      
  
      
      # Update the e_df data frame to be a tibble that contains only the variables we care about
      full_tibble <- as_tibble(e_df) %>%
        dplyr::select(qc(
          Eprime.Basename, Eprime.LevelName,
          ImageFile, Choose.OnsetTime,
          Choose.RTTime, Choose.RT, 
          VideoFile, MoveVid.OnsetTime
        ))
      
      
      
      # Create an empty tibble where a for loop creates repetitions of trial_proc rows to be used in adding to 
      # the full tibble as new columns ( so for each row, it will now have not only movement data, 
      # but also what trial type it is with Start and end goals, etc)
      trial_cols <- tibble()
      trial_proc_location <- which(e_df$Procedure == "TrialProc" | e_df$Procedure == "TrialRevProc")
      place <- 0
      i = 1
      
      for (row_location in 1:length(trial_proc_location)){
        repeat_num <- trial_proc_location[row_location] - place - 1
        curr_row <- trial_proc[row_location, ]
        new_rows <- curr_row[rep(seq_len(nrow(curr_row)), each = repeat_num), ]
        trial_cols <- rbind(trial_cols, new_rows)
        place <- trial_proc_location[row_location]
        i <- i + 1
       
      }
      
      
      
      
      
      # Remove TrialProc rows from full tibble, as well as the last row (which is useless), then column bind the new 
      # trial_cols(which should now have the same # of rows as the full tibble) to the full tibble
      full_tibble <- full_tibble[-c(trial_proc_location), ]
      full_tibble <- full_tibble[-(nrow(full_tibble)), ]
      full_tibble <- cbind(trial_cols, full_tibble)
      
      
      # Create a data frame with same number of rows as full tibble, that contains 3 columns of repeating values: 
      # Subject (participant #), Task (Explore | Test), TaskType (Explore: 1 or 2 | Test: A1...C3)
      Task <- strsplit(e_df_header$DataFile.Basename, "_")[[1]][1]
      Subject <- e_df_header$Subject
        if (e_df_header$Subject == 1 | e_df_header$Subject == 052){
          print("-------------------issue here - row 134-------------------")
        }
      Task_type <- strsplit(e_df_header$DataFile.Basename, "-")[[1]][1]
      Task_type <- paste0(strsplit(Task_type, "_")[[1]][3], strsplit(Task_type, "_")[[1]][4])
      Session_time <- e_df_header$SessionTime
      
      # Grab the get ready time of procedure
      if(!is.null(e_frame[length(e_frame)][[1]]["GetReady.RTTime"][[1]])){
        GetReady.RTTime <- as.integer(e_frame[length(e_frame)][[1]]["GetReady.RTTime"][[1]])
      } else {
        GetReady.RTTime <- NA
      }
      
      # Grab the finish time of procedure
      if(!is.null(e_frame[length(e_frame)][[1]]["Finished.OnsetTime"][[1]])){
        Finished.OnsetTime <- as.integer(e_frame[length(e_frame)][[1]]["Finished.OnsetTime"][[1]])
      } else {
        Finished.OnsetTime <- NA
      }
      
      # Grab the finish time of procedure
      # Finished.OnsetTime <- as.integer(e_frame[length(e_frame)][[1]]["Finished.OnsetTime"][[1]])
      # Finished.OnsetTime <- rep(Finished.OnsetTime, nrow(full_tibble))
      
      
      meta_df <- cbind(Subject, Task, Task_type, Session_time, GetReady.RTTime, Finished.OnsetTime)
      meta_df <- meta_df[rep(seq_len(nrow(meta_df)), each = nrow(full_tibble)), ]
      

      # Bind that meta data frame to the full tibble
      full_tibble <- cbind(meta_df, full_tibble)
      
        
    } 
    else {
      # Update the e_df data frame to be a tibble that contains only the variables we care about
      full_tibble <- as_tibble(e_df) %>%
        dplyr::select(qc(
          Eprime.Basename, Eprime.LevelName,
          ImageFile, Choose.OnsetTime, 
          Choose.RTTime, Choose.RT, 
          VideoFile, MoveVid.OnsetTime
        ))
      
      
      # Create a data frame with same number of rows as full tibble, that contains 3 columns of repeating values: 
      # Subject (participant #), Task (Explore | Test), TaskType (Explore: 1 or 2 | Test: A1...C3)
      Task <- strsplit(e_df_header$DataFile.Basename, "-")[[1]][1]
      Subject <- e_df_header$Subject
      Task_type <- strsplit(e_df_header$DataFile.Basename, "-")[[1]][3]
      Procedure <- NA
      Sample <- NA
      Session_time <- e_df_header$SessionTime
      itilist <- NA
      ITIDur <- NA
      ITI.OnsetTime <- NA
      objlist <- NA
      ObjDur <- NA
      pairlist <- NA
      startPosition <- NA
      startFacing <- NA
      StartIm <- NA
      endPosition <- NA
      EndIm <- NA
      StartAt.OnsetTime <- NA
      Goal.OnsetTime <- NA
      # Grab the get ready time of procedure
      if(!is.null(e_frame[length(e_frame)][[1]]["GetReady.RTTime"][[1]])){
        GetReady.RTTime <- as.integer(e_frame[length(e_frame)][[1]]["GetReady.RTTime"][[1]])
      } else {
        GetReady.RTTime <- NA
      }
      # Grab the finish time of procedure
      if(!is.null(e_frame[length(e_frame)][[1]]["Finished.OnsetTime"][[1]])){
        Finished.OnsetTime <- as.integer(e_frame[length(e_frame)][[1]]["Finished.OnsetTime"][[1]])
      } else {
        Finished.OnsetTime <- NA
      }
      
      
      meta_df <- cbind(Subject, Task, Task_type, Procedure, Sample, Session_time, GetReady.RTTime, Finished.OnsetTime, itilist, ITIDur, ITI.OnsetTime, objlist, ObjDur, pairlist, startPosition, startFacing, StartIm, endPosition, EndIm, StartAt.OnsetTime, Goal.OnsetTime)
      meta_df <- meta_df[rep(seq_len(nrow(meta_df)), each = nrow(full_tibble)), ]
      
      
      # Bind that meta data frame to the full tibble
      full_tibble <- cbind(meta_df, full_tibble)
      
        
    }
    

    
    # Append full_tibble to participant_full file through rbind()
    full_participant <- rbind(full_participant, full_tibble)

    
  }
  
  # Append full_participant file to the full_file
  full_file <- rbind(full_file, full_participant)
  
}



convert_to_pos <- function (imagecol){
  end_pos = c()
  for (i in 1:length(imagecol)){
    pos <- strsplit(as.character(imagecol[i]), "/")
    pos <- pos[[1]][2]
    pos <- strsplit(pos, ".jpg")
    pos <- pos[[1]][1]
    # TODO:  If string contains "circle", split
    end_pos <- c(end_pos, pos)
    
  }
  return(end_pos)
}

convert_to_hall <- function(videocol){
  hallsnip <- c()
  for (i in 1:length(videocol)){
    hall <- substr(videocol[i], 8, 12)
    hallsnip <- c(hallsnip, hall)
  }
  return(hallsnip)
}

convert_to_movement <- function(hallcol){
  movement <- c()
  for ( i in 1:length(hallcol)){
    
    mov <- substr(hallcol[i], 1, 1) == substr(hallcol[i], 4, 4)
    if (mov & !is.na(mov)){
      mov <- "Rot"
    } else if (hallcol[i] == "Selec" & !is.na(mov)){
      mov <- "Select"
    } else if (!is.na(mov)){
      mov <- "Walk"
    }
    movement <- c(movement, mov)
  }
  return(movement)
}

convert_to_letter <- function(endloccol){
  lett <- c()
  for (i in 1:length(endloccol)){
    let_loc <- substr(endloccol[i], 1, 1)
    lett <- c(lett, let_loc)
  }
  return(lett)
}

convert_to_dir <- function(endloccol){
  direction <- c()
  for (i in 1:length(endloccol)){
    dirnum <- as.numeric(substr(endloccol[i], 2, 2))
    if (!is.na(dirnum)){
      face_dir <- switch(dirnum, "N", "E", "S", "W")
    } else {
      face_dir <- dirnum
    }
    
    direction <- c(direction, face_dir)
  }
  return(direction)
}

endloc_fix <- function(endloccol){
  end_location <- c()
  for (i in 1:length(endloccol)){
    if (as.character(endloccol[i]) == "V3" & !is.na(endloccol[i])){
      el <- "Y3"
    } else if (as.character(endloccol[i]) == "V4" & !is.na(endloccol[i])){
      el <- "Y4"
    } else if (as.character(endloccol[i]) == "V3_sphere" & !is.na(endloccol[i])){
      el <- "Y3_sphere"
    } else {
      el <- as.character(endloccol[i])
    }
    end_location <- c(end_location, el)
  }
  return(end_location)
}

hallsnip_fix <- function(hallsnipcol){
  hallcol <- c()
  for (i in 1:length(hallsnipcol)){
    if (as.character(hallsnipcol[i]) == "V2_V3" & !is.na(hallsnipcol[i])){
      el <- "V2_Y3"
    } else if (as.character(hallsnipcol[i]) == "V2_V4" & !is.na(hallsnipcol[i])){
      el <- "Y2_Y4"
    } else if (as.character(hallsnipcol[i]) == "V4_V1" & !is.na(hallsnipcol[i])){
      el <- "Y4_V1"
    } else {
      el <- as.character(hallsnipcol[i])
    }
    hallcol <- c(hallcol, el)
  }
  return(hallcol)
}


full_file <- full_file %>% mutate(end_location = convert_to_pos(ImageFile))
print("End Location calculated and added...")
full_file <- full_file %>% mutate(hallsnip = convert_to_hall(VideoFile))
print("Hall video snippet calculated and added...")


full_file <- full_file %>% mutate(end_location = endloc_fix(end_location))
print("End location V & Y values fixed...")
full_file$end_location <- str_replace(full_file$end_location, "P1", "Z1")
full_file$end_location <- str_replace(full_file$end_location, "P4", "Z2")

full_file <- full_file %>% mutate(hallsnip = hallsnip_fix(hallsnip))
print("Hall snip video V & Y values fixed...")
full_file$hallsnip <- str_replace(full_file$hallsnip, "P1", "Z1")
full_file$hallsnip <- str_replace(full_file$hallsnip, "P4", "Z2")

full_file <- full_file %>% mutate(movement = convert_to_movement(hallsnip))
print("Movement type calculated and added...")

full_file <- full_file %>% mutate(face_dir = convert_to_dir(end_location))
print("Facing direction calculated and added...")
full_file <- full_file %>% mutate(sphere = grepl("sphere", full_file$end_location))
print("Next to Sphere boolean calculated and added...")



full_file <- full_file %>% mutate(letter_loc = convert_to_letter(end_location))
print("Letter-End Location calculated and added...")

mcoords <- read.csv("location.csv")
colnames(mcoords)[1] <- "letter_loc"

full_file <- join(full_file, mcoords, by = "letter_loc")
print("X and Y coordinates merged and added...")

write.csv(full_file, "MLINDIV_behavioral_full.csv")



#### 

full_file_edit <- read.csv("MLINDIV_behavioral_full.csv")
sub_052 <- which(full_file_edit$Subject == 1 & full_file_edit$Task_type == "C4")
full_file_edit$Subject[sub_052] = 52
sub_076 <- which(full_file_edit$Subject == 1 & full_file_edit$Task_type == "B1")
full_file_edit$Subject[sub_076] = 76
print("Fixed subject '1' errors for subjects 52 and 76...")

sub_113 <- which(full_file_edit$Subject == 20 & full_file_edit$Task_type == "1")
sub_113 <- sub_113[sub_113 > 90000]
full_file_edit$Subject[sub_113] = 113
full_file_edit$Task_type[sub_113] = 2
print("Fixed subject '20' run '1' errors for subject 113 run 2...")

write.csv(full_file_edit[-c(1)], "MLINDIV_behavioral_full.csv")
