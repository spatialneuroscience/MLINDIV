meta_generator <- function(mass_behav, trial_mass, folder_name, useless_list){

  dir.create(folder_name)
  total_unique_list <- as.data.frame(unique(mass_behav$Subject))
  names(total_unique_list) <- c('subj')
  total_unique_list <- filter(total_unique_list, !total_unique_list$subj %in% useless_list)

  for (i in total_unique_list$subj){
    subj = i
    path <- folder_name
    single_subject_file_generator(subj, mass_behav, trial_mass, path)
  }
}
