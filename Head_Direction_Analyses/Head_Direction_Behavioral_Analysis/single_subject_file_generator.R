single_subject_file_generator <- function(subj, mass, trial_mass, path){
  sub_folder <- paste0(path,'/','sub',subj)
  print(sub_folder)
  dir.create(sub_folder)
  behav <- mass %>% filter(Subject == subj)
  behav_trial <- trial_mass %>% filter(Subject == subj)
  for (i in unique(behav$Task_type)){
    # generate explore
    if (i == 1 || i == 2){
      behave_exp1 <- behav %>% 
        filter(`Task_type` == i)
      behave_exp1_trial <- behav_trial %>% 
        filter(`Task_type` == i)
      exploration1_file <- exploration_file_generator(behave_exp1, behave_exp1_trial)
      file_name <- paste0(sub_folder, '/', 'sub',subj,'_exp',i, '.csv')
      write_csv(exploration1_file, file_name)
    }else{
      behave_test1 <- behav %>% 
        filter(`Task_type` == i)
      behave_test1_trial <- behav_trial %>% 
        filter(`Task_type` == i)
      test1_file <- test_single_run_generator(behave_test1, behave_test1_trial)
      run_num <- as.character(behave_test1_trial$run_num[1]) # strsplit(as.character(i),"")[[1]][2]
      file_name <- paste0(sub_folder, '/','sub',subj,'_test',run_num, '.csv')
      write_csv(test1_file, file_name)
    }
  }
}

