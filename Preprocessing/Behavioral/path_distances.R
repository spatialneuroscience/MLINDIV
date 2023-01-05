# 3
# Original Author: Robert Woodry
# FINAL Version Adapted By: Alina Tu
# Contact: alinat2@uci.edu
# Last Updated: 1/4/2023

# Changes in FINAL Version: New working_dir path, corrected path efficiency scoring to include incorrect trials in addition
#         to correct trials, "master" -> "full,"
#   [1/4/23 changes]    extra column removed
# Rob's original script is now in /mnt/chrastil/lab/users/rob/scripts/MLINDIV/OldVersions/
# Rob's output data is now in /mnt/chrastil/lab/data/MLINDIV/raw/raw_behav/OldVersions/

library(rprime)

working_dir <- "/mnt/chrastil/lab/data/MLINDIV/raw/raw_behav/"
setwd(working_dir)

pd <- read.csv("pathdistances.csv")
loc <- read.csv("location.csv")
pd$node_pairs <- paste0(pd$n1, pd$n2)
path_distance <- 0


sum_path_dist <- function(pathscol){
  path_distances <- c()
  error_list <- c()
  for (i in 1:length(pathscol)){
    path_distance <- 0
    path <- strsplit(as.character(pathscol[i]), " ")[[1]]

    for (j in 1:(length(path) - 1)){
      node_pair <- paste0(path[j], path[j + 1])
      if (node_pair %in% pd$node_pairs){
        path_dist <- pd$distance[which(pd$node_pairs == node_pair)]
      } else if (path[j] == path[j+1]){
        path_dist <- 0
      } else {
        path_dist <- 0
        error_list <- c(error_list, node_pair)
      }
      path_distance <- path_distance + path_dist
      error_list <- error_list
      
    }
    path_distances <- c(path_distances, path_distance)
    error_list <- error_list
    
  }
  # return(error_list)
  # Uncomment the above line and comment the below line to see where the errors in calculating node pair distances are occuring
  
  return(path_distances)
}

calc_euc_dist <- function(pathscol){
  euc_dist_trav <- c()
  for (i in 1:length(pathscol)){
    path <- strsplit(as.character(pathscol[i]), " ")[[1]]
    start <- path[1]
    end <- path[length(path)]
    x_start <- loc[loc$Location == start, 2]
    y_start <- loc[loc$Location == start, 3]
    x_end <- loc[loc$Location == end, 2]
    y_end <- loc[loc$Location == end, 3]
    
    x_diff <- x_end - x_start
    y_diff <- y_end - y_start

    euc_dist <- sqrt(sum(x_diff^2, y_diff^2))
    euc_dist_trav <- c(euc_dist_trav, euc_dist)
  }
  return(euc_dist_trav)
  
}

tm <- read.csv("MLINDIV_trial_full.csv")
tm <- tm %>% mutate(path_dist_trav = sum_path_dist(paths))
tm <- tm %>% mutate(euc_dist_trav = calc_euc_dist(paths))

write.csv(tm, "MLINDIV_trial_full.csv")


pd <- read.csv("pathdistances.csv")

pd$node_pairs <- paste0(pd$n1, pd$n2)

calc_pd <- function(pathscol){
  path_distances <- c()
  for (i in 1:length(pathscol)){
    path_distance <- 0
    path <- strsplit(as.character(pathscol[i]), " ")[[1]]
    
    for (j in 1:(length(path) - 1)){
      node_pair <- paste0(path[j], path[j + 1])
      if (node_pair %in% pd$node_pairs){
        path_dist <- pd$distance[which(pd$node_pairs == node_pair)]
      } else if (path[j] == path[j+1]){
        path_dist <- 0
      } else {
        path_dist <- 0

      }
      path_distance <- path_distance + path_dist

      
    }
    path_distances <- c(path_distances, path_distance)
  }
    

    return(path_distances)
}

tm <- read.csv("MLINDIV_trial_full.csv")

path_efficiencies <- c()
tm$paths <- as.character(tm$paths)

# Calculate path efficiencies
for (i in 1:nrow(tm)){
  path_eff <- 0
  if (!is.na(tm$Procedure[i]) & !is.na(tm$select_made[i])){
    path_eff <- tm$path_dist_trav[i] / tm$Path.Distance[i]
    # path_eff <- tm$path_dist_trav[i] / tm$Path.Distance[which(tm$StartAt == tm$StartAt[i] & tm$EndAt == as.character(tm$end_location[i]))[1]]
  }
  else {
    path_eff <- NA
  }
  
  if (tm$StartAt[i] == tm$end_location[i] & !is.na(tm$StartAt[i])){
    path_eff <- NA
    
    if (length(strsplit(as.character(tm$paths[i]), " ")[[1]]) == 2){
      
      tm$paths[i] <- strsplit(as.character(tm$paths[i]), " ")[[1]][2]
    }
  }
  path_efficiencies <- c(path_efficiencies, path_eff)
}

tm_new <- cbind(tm, path_efficiencies)

# Grab Time Stamp of experiment session
dates <- c()
times <- c()

for (i in 1:nrow(tm_new)){
  filename <- sprintf("%03d/%s.txt", tm$Subject[i], tm$eprocs[i])
  print(filename)

  ss_date <- read_eprime(filename)[14]
  ss_time <- read_eprime(filename)[15]

  
  dates <- c(dates, ss_date)
  times <- c(times, ss_time)

}
for (time in 1:length(times)){
  times[time] <- as.character(strsplit(as.character(times[time]), " ")[[1]][2])
}

tm_new <- cbind(dates, times, tm_new)
tm_new <- tm_new[, c(3, 1:2, 4:ncol(tm_new))]

tm_new$dates <- as.Date(tm_new$dates, "SessionDate: %m-%d-%Y")



write.csv(tm_new[-c(1)], "MLINDIV_trial_full.csv", row.names = FALSE)
