stationary_dir <- function(label){
  label <- as.character(label)
  label1 <- strsplit(label, split = character(0))
  label2 <- label1[[1]][2]
  
  if (label2 == "1"){
    label3 = 'N'
  } else if(label2 == "2"){
    label3 = 'E'
  } else if(label2 == "3"){
    label3 = 'S'
  } else {label3 = 'W'}
  
  return(label3)
}

translation_type <- function(label){
  label <- as.character(label)
  label1 <- strsplit(label, split = character(0))
  label2a <- label1[[1]][2]
  label2b <- label1[[1]][5]
  
  if (label2a == label2b){
    event = 'Walk'
  } else {event = 'Rot'}
  
  return(event)
}

ego_rot_dir <- function(label){
  label <- as.character(label)
  label1 <- strsplit(label, split = character(0))
  label2a <- label1[[1]][2]
  label2b <- label1[[1]][5]
  
  if ((label2a == "1" && label2b == "2") || (label2a == "2" && label2b == "3")||(label2a == "3" && label2b == "4") || (label2a == "4" && label2b == "1")|| (label2a == "1" && label2b == "3")|| (label2a == "2" && label2b == "4")){ 
    label3 = 'R'
  } else {label3 = 'L'}

  return(label3)
}

allo_rot_dir <- function(label){
  label <- as.character(label)
  label1 <- strsplit(label, split = character(0))
  label2a <- label1[[1]][2]
  label2b <- label1[[1]][5]
  
  if (label2a == "1" && label2b == "2"){
  label3 = 'NE'
  }else if(label2a == "2" && label2b == "3"){
  label3 = 'ES'
  }else if(label2a == "3" && label2b == "4"){
    label3 = 'SW'
  }else if(label2a == "4" && label2b == "1"){ 
    label3 = 'WN'
  } else if(label2a == "1" && label2b == "4"){
    label3 = 'NW'
  }else if(label2a == "4" && label2b == "3"){
    label3 = 'WS'
  }else if(label2a == "3" && label2b == "2"){ 
    label3 = 'SE'
  }else if(label2a == "1" && label2b == "3"){ 
    label3 = 'NS'
  }else if(label2a == "3" && label2b == "1"){ 
    label3 = 'SN'
  }else if(label2a == "2" && label2b == "4"){ 
    label3 = 'EW'
  }else if(label2a == "4" && label2b == "2"){ 
    label3 = 'WE'
  } else{label3 = 'EN'}

  return(label3)
}

allo_walk_dir <- function(label){
  label <- as.character(label)
  label1 <- strsplit(label, split = character(0))
  label2 <- label1[[1]][5]
  
  if (label2 == "1"){
    label3 = 'N'
  } else if(label2 == "2"){
    label3 = 'E'
  } else if(label2 == "3"){
    label3 = 'S'
  } else {label3 = 'W'}
  
  return(label3)
}
