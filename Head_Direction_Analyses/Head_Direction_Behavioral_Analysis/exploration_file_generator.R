exploration_file_generator <- function(behave_exp1, behave_exp1_trial){
  
  dat <- data.frame('sub' = rep(behave_exp1$Subject[1], 2*(nrow(behave_exp1)-2)))
  
  for (i in 1:(nrow(behave_exp1)-2)){
    
    # stationary line
    
    dat$event[(2*i-1)] = 'Stationary'  # event
    dat$ego_dir[(2*i-1)] = 'F' # ego-direction
    dat$allo_dir[(2*i-1)] = stationary_dir(behave_exp1$hallsnip[i]) # allo-direction
    dat$onset[(2*i-1)] = behave_exp1$Choose.OnsetTime[i]/1000 - behave_exp1_trial$getready_rttime[1]/1000 # onset
    dat$duration[(2*i-1)] = (behave_exp1$MoveVid.OnsetTime[i] - behave_exp1$Choose.OnsetTime[i])/1000 #duration
    
    # movement line
    dat$event[(2*i)] = as.character(translation_type(behave_exp1$hallsnip[i])) # event
    
    if (dat$event[(2*i)]  == 'Rot'){ 
      dat$ego_dir[(2*i)] = ego_rot_dir(behave_exp1$hallsnip[i])# ego-direction
      dat$allo_dir[(2*i)] =  allo_rot_dir(behave_exp1$hallsnip[i]) # allo-direction
    } else{ 
      dat$ego_dir[(2*i)] = 'F' # ego-direction
      dat$allo_dir[(2*i)]= allo_walk_dir(behave_exp1$hallsnip[i]) # allo-direction
    }
    
    dat$onset[(2*i)] = behave_exp1$MoveVid.OnsetTime[i]/1000 - behave_exp1_trial$getready_rttime[1]/1000  # onset
    dat$duration[(2*i)] = (behave_exp1$MoveVid.OnsetTime[i+1] - behave_exp1$Choose.OnsetTime[i])/1000 #duration
    
  }
  
  dat$session = behave_exp1$Task_type[1]
  return(dat)
}
