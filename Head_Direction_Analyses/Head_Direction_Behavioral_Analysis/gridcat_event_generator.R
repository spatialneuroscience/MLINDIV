gridcat_direction <- function(direction){
  if (direction == "N"){return(0)}else if(direction == "E"){return(90)}else if(direction == "S"){return(180)}else{270}
}

gridcat_event_generator <- function(subj, input_folder, output_folder){
  
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
    exp1 = read_csv(i)
    new_exp <- exp1 %>% 
      filter(event == 'Walk' | event == 'Stationary') %>% 
      mutate(allo_dir = gridcat_direction(allo_dir)) %>% 
      select(event, onset, duration, allo_dir)
    
    sub_name = subj

    
    if (idx == -1){
      str_name = paste0(sub_folder, '/sub-', sub_name, '_explore1.txt')
    }else if(idx == 0){
      str_name = paste0(sub_folder, '/sub-', sub_name, '_explore2.txt')
    }else{
      str_name = paste0(sub_folder, '/sub-', sub_name, '_test', idx, '.txt')
    }
    
    write.table(new_exp, file = str_name,sep = ";", row.names = FALSE,
                col.names = FALSE, quote = FALSE)
    
  }
  
}
