translation_allo <- function(i, delay){
  exp1 = read_csv(i)
  exp1_allo <- exp1 %>% 
    filter(event == 'Walk') %>% 
    mutate(onset = onset + delay) %>% 
    select(onset, allo_dir, duration)
  names(exp1_allo) <- c('onset', 'trial_type','duration')
  print(exp1_allo)
  return(exp1_allo)
}

stationary_allo <- function(i, delay){
  exp1 = read_csv(i)
  exp1_allo <- exp1 %>% 
    filter(event == 'Stationary') %>% 
    mutate(onset = onset + delay) %>% 
    select(onset, allo_dir, duration)
  names(exp1_allo) <- c('onset', 'trial_type','duration')
  return(exp1_allo)
}


betaseries_event_raw <- function(subj, input_folder, output_folder, event_type, delay = 0){
  
  # create subject-specific output folder
  sub_folder <- paste0(output_folder,'/','sub',subj)
  dir.create(sub_folder)
  
  # set path to read files from the input folder
  path <- paste0(input_folder,'/sub',subj,'/')
  exp1_path <- paste0(path, 'sub',subj,'_exp1.csv')
  exp2_path <- paste0(path, 'sub',subj,'_exp2.csv')
  test1_path <- paste0(path, 'sub',subj,'_test1.csv')
  test2_path <- paste0(path, 'sub',subj,'_test2.csv')
  test3_path <- paste0(path, 'sub',subj,'_test3.csv')
  test4_path <- paste0(path, 'sub',subj,'_test4.csv')
  test5_path <- paste0(path, 'sub',subj,'_test5.csv')
  test6_path <- paste0(path, 'sub',subj,'_test6.csv')
  
  filename_list <- c(exp1_path, exp2_path, test1_path, test2_path, test3_path, test4_path, test5_path, test6_path)
  
  idx <- -2
  for (i in filename_list){
    idx = idx + 1
    new_exp = event_type(i, delay)
    # rename subject name in the file e.g., 33 - 033, 3 - 003, 
    if (subj < 10){sub_name = paste0('00',subj)}else if(subj >= 10 & subj < 100){sub_name = paste0('0',subj) }else{sub_name = subj}
    
    if (idx == -1){
      str_name = paste0(sub_folder, '/sub-', sub_name, '_task-boldEx_run-1_events.tsv')
    }else if(idx == 0){
      str_name = paste0(sub_folder, '/sub-', sub_name, '_task-boldEx2_run-1_events.tsv')
    }else{
      str_name = paste0(sub_folder, '/sub-', sub_name, '_task-boldRun', idx, '_run-1_events.tsv')
    }
    
    write.table(new_exp, file = str_name,sep = ",", row.names = FALSE,
                col.names = TRUE, quote = FALSE)
    
  }
  
}
