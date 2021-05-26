working_dir <- "C:/Users/UCI - Robert Woodry/Desktop/Research/Tasks/MLINDIV/EPrime Experiment Files/Data/BehavPreJustin"
setwd(working_dir)
# TODO: Need to add Explore Trials completed tally and Test trials completed tally

tm <- read.csv("MLINDIV_trial_master.csv")

tm_split <- split(tm, tm$Subject)

options(scipen = 999)

DOI <- c('accuracy', 'select_made', 'nodesturns_count', 'nodes_count', 
         'turns_count', 'average_RT', 'trial_duration', 'path_dist_trav', 'euc_dist_trav', 'path_efficiencies')

objlmarks <- c("obj_guitar", "obj_snowman", "obj_lamp", "obj_spaceship", 
                       "obj_chair", "obj_chicken", "obj_trophy", "obj_umbrella", 
                       "obj_clock", "lm_starrynight", "lm_bwstair", "lm_person",
                       "lm_philsbar")

objlmarks <- sort(objlmarks)

starrynight <- c("D1", "E4", "F4", "J1", "R1", "U1", "C2", "B2")
bwstair <- c("H3", "Q4", "R4", "S4", "T1", "B3")
person <- c("M3", "Q2", "R2", "S2", "E3")
philsbar <- c("U3", "X2", "T2", "R3", "P4", "J3", "D3")

pm_colnames <- c("Subject", "StartDate", "StartTime", paste0("tm_", DOI), paste0("ts_", DOI), paste0("em_", DOI), paste0("es_", DOI), paste0("enodes_", LETTERS), objlmarks)

participant_master <- c()

acc_mean <- c()
acc_sd <- c()

rt_mean <- c()
rt_sd <- c()

n_trials <- c()
n_explores <- c()
n_selects <- c()
n_corrects <- c()
n_outoftimes <- c()

for (i in 1:length(tm_split)){
  # Node Visits during Explore sessions

  explore <- tm_split[[i]][grepl("Explore", tm_split[[i]]$eprocs), ]  # For each subject, grab the explore session trial data
  trials <- tm_split[[i]][grepl("Test", tm_split[[i]]$eprocs), ]
  
  oall_node_table <- table(LETTERS)
  oall_node_table[LETTERS] <- 0
  
  obj_lmark_table <- table(objlmarks)
  obj_lmark_table[objlmarks] <- 0
  
  print(i)
  
  if (nrow(explore) != 0){
    for (j in 1:nrow(explore)){                                         # For each explore trial session, tally into a table the nodes visited and sum them into one general explore table
    path_node_table <- table(factor(strsplit(as.character(explore$paths[j]), " ")[[1]], levels = LETTERS))
    oall_node_table <- oall_node_table[LETTERS] + path_node_table[LETTERS]
    
    ex_table <- table(strsplit(as.character(explore$e_paths[j]), " ")[[1]])
    
    obj_lmark_table["obj_guitar"] <-sum(obj_lmark_table["obj_guitar"], ex_table["A4"], na.rm = TRUE)
    obj_lmark_table["obj_snowman"] <- sum(obj_lmark_table["obj_snowman"], ex_table["I2"], na.rm=TRUE)
    obj_lmark_table["obj_lamp"] <- sum(obj_lmark_table["obj_lamp"], ex_table["K2"], na.rm = TRUE)
    obj_lmark_table["obj_spaceship"] <- sum(obj_lmark_table["obj_spaceship"], ex_table["L2"], na.rm = TRUE)
    obj_lmark_table["obj_chair"] <- sum(obj_lmark_table["obj_chair"], ex_table["P2"], na.rm=TRUE)
    obj_lmark_table["obj_chicken"] <- sum(obj_lmark_table["obj_chicken"], ex_table["F3"], na.rm=TRUE)
    obj_lmark_table["obj_trophy"] <- sum(obj_lmark_table["obj_trophy"], ex_table["O2"], na.rm=TRUE)
    obj_lmark_table["obj_umbrella"] <- sum(obj_lmark_table["obj_umbrella"], ex_table["V2"], na.rm=TRUE)
    obj_lmark_table["obj_clock"] <- sum(obj_lmark_table["obj_clock"], ex_table["W4"], na.rm=TRUE)
    obj_lmark_table["lm_starrynight"] <- sum(obj_lmark_table["lm_starrynight"], ex_table[starrynight], na.rm=TRUE)
    obj_lmark_table["lm_bwstair"] <- sum(obj_lmark_table["lm_bwstair"], ex_table[bwstair], na.rm=TRUE)
    obj_lmark_table["lm_person"] <- sum(obj_lmark_table["lm_person"], ex_table[person], na.rm=TRUE)
    obj_lmark_table["lm_philsbar"] <- sum(obj_lmark_table["lm_philsbar"], ex_table[philsbar], na.rm=TRUE)

    }
  }
  two <- tm_split[[i]]
  n_trial <- nrow(two[grepl("Test", two$eprocs), ])
  n_explore <- nrow(two[grepl("Explore", two$eprocs), ])
  n_select <- sum(two$select_made, na.rm = TRUE)
  n_correct <- sum(two$accuracy, na.rm = TRUE)
  n_outoftime <- sum(!two$select_made, na.rm = TRUE)

  n_trials <- c(n_trials, n_trial)
  n_explores <- c(n_explores, n_explore)
  n_selects <- c(n_selects, n_select)
  n_corrects <- c(n_corrects, n_correct)
  n_outoftimes <- c(n_outoftimes, n_outoftime)



  
  trial_means <- c()
  trial_sds <- c()
  explore_means <- c()
  explore_sds <- c()
 
  for (j in 1:length(DOI)){
    trial_means <- c(trial_means, mean(trials[DOI[j]][ ,1], na.rm = TRUE))
    trial_sds <- c(trial_sds, sd(trials[DOI[j]][ ,1], na.rm = TRUE))
    explore_means <- c(explore_means, mean(explore[DOI[j]][ ,1], na.rm = TRUE))
    explore_sds <- c(explore_sds, sd(explore[DOI[j]][ ,1], na.rm = TRUE))
  }

  p_master <- c(as.character(tm_split[[i]]$Subject[1]), as.character(tm_split[[i]]$dates[1]), as.character(tm_split[[i]]$times[1]), trial_means, trial_sds, explore_means, explore_sds, oall_node_table, obj_lmark_table)
  participant_master <- rbind(participant_master, p_master)  
  
}

colnames(participant_master) <- pm_colnames
participant_master <- participant_master[, c(1:23, 26:33, 36:ncol(participant_master))]
participant_master <- cbind(n_trials, n_explores, n_corrects, n_selects, n_outoftimes, participant_master)
participant_master <- participant_master[, c(6, 1:5, 7:ncol(participant_master))]   # Reorder columns

write.csv(participant_master, "MLINDIV_participant_master.csv", row.names = FALSE)
