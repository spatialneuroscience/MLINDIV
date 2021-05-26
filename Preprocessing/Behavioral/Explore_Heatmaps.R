# This script requires ggplot. 
# If you do not have ggplot, do install.packages('tidyverse')
library(ggplot2)


# Set Working Directory to MLINDIV master files
working_Dir <- "C:/Users/17868/Documents/R/MLINDIV"
setwd(working_Dir)

# Read in master files
bm <- read.csv("MLINDIV_behavioral_master.csv")
tm <- read.csv("MLINDIV_trial_master.csv")
pm <- read.csv("MLINDIV_participant_master.csv")

# For each subject's explore session, split behavioral data into time windows of window width window_width
#   and of step length window_by. 
#   
#   explore_windows Function outputs to both the R workspace and to a STW_[sessiontype]_[window_width]_[window_by].csv file
#   Example: output <- explore_windows(bm, "Explore", 36000, 34560) 
#     - This creates both an output variable within the R workspace containing the newly made data frame,
#       and a STW_Explore_36000_34560.csv containing the newly made data frame within the current working directory

explore_windows <- function(data, sessiontype, window_width, window_by){
  
  # Create empty data frame where each row will contain a sliding time window of explore session data
  # TODO: define variables for ex_window data frame
  ex_windows <- data.frame(
    subject = integer(),
    session = character(),
    window_iter = integer(),
    window_length = numeric(),
    window_start = numeric(),
    x = numeric(),
    y = numeric(),
    letter_loc = character(),
    face_dir = character(),
    choose_onset = numeric(),
    choose_rt_time = numeric(),
    choose_reactiontime = numeric(),
    num_moves = numeric(),
    num_turns = numeric(),
    num_north = numeric(),
    num_south = numeric(),
    num_west = numeric(),
    num_east = numeric(),
    mean_reactiontime = numeric(),
    sd_reactiontime = numeric(),
    stringsAsFactors = FALSE
    
  )
  
  # Select only explore session data
  data <- data[which(data$Task == sessiontype), ]
  
  # Split behavioral master by subject
  split_by_subject <- split(data, data$Subject)
  iteration <- 0
  
  for (i in 1:length(split_by_subject)){
    subject_data <- split_by_subject[[i]]
    
    subject_split_by_session <- split(subject_data, subject_data$Task_type)
    subject_split_by_session <- subject_split_by_session[sapply(subject_split_by_session, function(x) dim(x)[1]) > 0]
    
    for (j in 1:length(subject_split_by_session)){
      # Split subject data into sessions, and for each one grab the choose onset times (wihtout NAs)
      print(paste("Running subject iteration: ", i, "session iteration: ", j))
      subject_session_data <- subject_split_by_session[[j]]
      subject_session_data <- subject_session_data[1:(nrow(subject_session_data)-1), ]
      
      sub_times <- subject_session_data$Choose.OnsetTime
      sub_times <- sub_times[!is.na(sub_times)]
      
      # Subdivide session into equal time steps of window_width every window_by. Get intial time and increment.
      initial_time <- sub_times[1]
      end_time <- sub_times[length(sub_times)]
      
      win_starts <- seq(initial_time, end_time, window_by)
      win_ends <- win_starts + window_width
      win_ends[length(win_ends)] <- sub_times[length(sub_times)]
      
      choose_times <- subject_session_data$Choose.OnsetTime
      ssd <- subject_session_data
      
      # TODO: Change NA values to 0 (for windows where path vector length == 1)
      # TODO: Merge new window data set with subject's accuracy rating
      
      for (k in 1:length(win_starts)){
        window_data <- ssd[(choose_times >= win_starts[k] & choose_times <= win_ends[k]), ]
        x <- paste(window_data$x, collapse = " ")
        y <- paste(window_data$y, collapse = " ")
        letter_loc <- paste(window_data$letter_loc, collapse = " ")
        face_dir <- paste(window_data$face_dir, collapse = " ")
        choose_OT <- paste(window_data$Choose.OnsetTime, collapse = " ")
        choose_RTT <- paste(window_data$Choose.RTTime, collapse = " ")
        choose_RT <- paste(window_data$Choose.RT, collapse = " ")
        
        
        window_row <- data.frame(ssd$Subject[1], ssd$Task_type[1], k, win_ends[k] - win_starts[k], win_starts[k],
                        x, y, letter_loc, face_dir, choose_OT, choose_RTT, choose_RT,
                        as.numeric(table(window_data$movement)['Walk']), 
                        as.numeric(table(window_data$movement)['Rot']),
                        as.numeric(table(window_data$face_dir)['N']),
                        as.numeric(table(window_data$face_dir)['S']),
                        as.numeric(table(window_data$face_dir)['W']),
                        as.numeric(table(window_data$face_dir)['E']),
                        mean(window_data$Choose.RT, na.rm = TRUE), sd(window_data$Choose.RT, na.rm = TRUE))
        
        iteration <- iteration + 1
        ex_windows[iteration, ] <- window_row
      }
    }
    
  }
  write.csv(ex_windows, sprintf("STW_%s_%i_%i.csv", sessiontype, window_width, window_by))
  return(ex_windows)

}

# Create a heatmap and 2d density plot given an explore_window and

mlindiv_heatmap <- function(ex_window, rows, output = FALSE, noise = 0.1){
  data <- data.frame(x = as.numeric(strsplit(paste(ex_window$x[rows], 
                                                   collapse = " "), " ")[[1]]),  
                     y = as.numeric(strsplit(paste(ex_window$y[rows], 
                                                   collapse = " "), " ")[[1]]),
                     id = 1:length(as.numeric(strsplit(
                       paste(ex_window$y[rows], collapse = " "), " ")[[1]])),
                     time = as.numeric(strsplit(paste(
                       ex_window$choose_onset[rows], collapse = " "), " ")[[1]])
                     )
  tw_index <- c()
  subject <- c()
  
  for (i in rows){
    tw_index <- c(tw_index, 
                  rep(ex_window$window_iter[i], 
                      (ex_window$num_turns[i] + ex_window$num_moves[i])))
    subject <- c(subject,
                 rep(ex_window$subject[i], 
                     (ex_window$num_turns[i] + ex_window$num_moves[i])))
  }
  
  data <- cbind(data, tw_index, subject)
  
  data <- merge(data, table(data[,1:2]), sort = FALSE)
  data <- data[order(data$id), ]
  
  #data$x <- jitter(data$x, noise)
  #data$y <- jitter(data$y, noise)
  
  
  if(output){
    return(data)
  }
  else{
    # TODO: option to plot Accuracy quartiles ~ time windows (four rows corresponding to acc quantiles, and the columns corresponding to time window index)
    ggplot(data, aes(x,y)) + 
      coord_fixed(xlim = c(-5.5, 8.5), ylim = c(-8,6)) +
      facet_grid(rows = vars(tw_index),cols=vars(subject)) +
      # stat_density_2d_filled() + 
      geom_density_2d_filled(aes(x=jitter(x,noise), y = jitter(y, noise)), contour_var = "ndensity") + 
      geom_path(aes(color = time), size=1) + 
      geom_point(size = 3, aes(color=time)) + 
      #geom_point(data = data, aes(x = data[1,1], y = data[1,2]),shape = 'S', size = 3, color = 'white') +
      #geom_point(data = data, aes(x=data[nrow(data), 1], y = data[nrow(data), 2]),shape = 'E', size = 3, color = 'white') +
      labs(color = "Time (ms)")
  }
  
}



