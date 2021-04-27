# count in scan time
library(tidyverse)
source(here::here('single_subject_file_generator.R'))
source(here::here('exploration_file_generator.R'))
source(here::here('test_file_generator.R'))
source(here::here('direction.R'))

sublist <- c(2,3,5,14,18,19,20,21,17,33,24,25,27,32,29,37,38,36,39,41,43)
mass <- read.csv("MLINDIV_behavioral_master_Dec30.csv", head = TRUE)
mass <- filter(mass, mass$Subject %in% sublist)

scan_time = read_csv('scan_time.csv')
per_scan_time = 750 # ms

check_test_type <- function(Task_type){
  num = as.numeric(strsplit(as.character(Task_type), '')[[1]][2])
  return(num)}

dir.create('tmp')

for (i in sublist){
  sub_dat = filter(mass, mass$Subject == i)
  sub_scan = filter(scan_time, scan_time$sub == i)
  
  sub_dat <- sub_dat %>% 
    mutate(scan_num = ifelse(Task_type == 1, sub_scan$exp[1], 
                             ifelse(Task_type == 2, sub_scan$exp2[1], 
                                    ifelse(Task == 'Test' & map_dbl(Task_type,check_test_type) == 1, sub_scan$test1[1], 
                                           ifelse(Task == 'Test' & map_dbl(Task_type,check_test_type) == 2, sub_scan$test2[1], 
                                                  ifelse(Task == 'Test' & map_dbl(Task_type,check_test_type) == 3, sub_scan$test3[1], 
                                                         ifelse(Task == 'Test' & map_dbl(Task_type,check_test_type) == 4, sub_scan$test4[1], 
                                                                ifelse(Task == 'Test' & map_dbl(Task_type,check_test_type) == 5, sub_scan$test5[1], sub_scan$test6[1])))))))) %>% 
    mutate(initial_time = Finished.OnsetTime - scan_num * per_scan_time)
  
  single_subject_file_generator(i, sub_dat, 'tmp')
  
}
