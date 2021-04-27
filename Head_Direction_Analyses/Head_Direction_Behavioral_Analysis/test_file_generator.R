test_single_trial_generator <- function(input, session, initial_time){
  behave_exp1 = input
  
  
  
  if (nrow(behave_exp1) >= 2){
    dat <- data.frame('sub' = rep(behave_exp1$Subject[1], 2*(nrow(behave_exp1)-1)))
    for (i in 1:(nrow(behave_exp1)-1)){
      # stationary line
      dat$event[(2*i-1)] = 'Stationary'  # event
      dat$ego_dir[(2*i-1)] = 'F' # ego-direction
      dat$allo_dir[(2*i-1)] = stationary_dir(behave_exp1$hallsnip[i]) # allo-direction
      dat$onset[(2*i-1)] = behave_exp1$Choose.OnsetTime[i]/1000 - initial_time # onset
      dat$duration[(2*i-1)] = (behave_exp1$MoveVid.OnsetTime[i] - behave_exp1$Choose.OnsetTime[i])/1000 #duration
      
      # movement line
      dat$event[(2*i)] = as.character(translation_type(behave_exp1$hallsnip[i])) # event
      
      if (dat$event[(2*i)] == 'Rot'){ 
        dat$ego_dir[(2*i)] = ego_rot_dir(behave_exp1$hallsnip[i])# ego-direction
        dat$allo_dir[(2*i)] =  allo_rot_dir(behave_exp1$hallsnip[i]) # allo-direction
      } else{ 
        dat$ego_dir[(2*i)] = 'F' # ego-direction
        dat$allo_dir[(2*i)]= allo_walk_dir(behave_exp1$hallsnip[i]) # allo-direction
      }
      
      dat$onset[(2*i)] = behave_exp1$MoveVid.OnsetTime[i]/1000 - initial_time # onset
      dat$duration[(2*i)] = (behave_exp1$MoveVid.OnsetTime[i+1] - behave_exp1$Choose.OnsetTime[i])/1000 #duration
      
    }
  }else{
  dat <- data.frame('sub' = behave_exp1$Subject[1])
  dat$event[1] = 'miss'
  dat$ego_dir[1] = 'NA'
  dat$allo_dir[1] = 'NA'
  dat$onset[1] = 'NA'
  dat$duration[1]= 'NA'
  }
  dat$session = session
  return(dat)
}

test_single_run_generator <- function(behave_test1, behave_test1_trial){
  
  block = as.numeric(strsplit(as.character(behave_test1$Task_type[1]),"")[[1]][2])*10
  
  run_dat <- data.frame()
  initial_time = behave_test1_trial$getready_rttime[1]/1000
  
  for (i in unique(behave_test1$Sample)){
    
    behave_test1_sub = behave_test1 %>% filter(Sample == i)
    session = block + i
    dat = test_single_trial_generator(behave_test1_sub, session, initial_time)
    run_dat = rbind(run_dat, dat)
  }
  return(run_dat)
}
