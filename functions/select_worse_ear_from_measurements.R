select_worse_ear_from_measurements = function(data, tests, test_betterWorse = NA, left = "L", right = "R"){
  
  # Function to select data from the worse ear across relevant measures 
  # requires audiogram AC to determine worse ear 
  
  # data = data.frame containing all relevant features
  # test = test conditions, for instance: c(AC, BC, UCL, acalos) 
  # test_betterWorse = tests (e.g. oto, val, tymp) translated to better or worse ear instead of left and right 
  # left = how left is coded, here "L"
  # right = same as with left
  
  test_list = {}
  
  # get FEATURE groups
  for (t in tests){
    
    test_list$R[[t]] = select(data, contains(paste0(t,"_",right)))
    test_list$L[[t]] = select(data, contains(paste0(t,"_",left)))
    
  }
  
  right_ear = do.call(cbind, test_list$R)
  left_ear = do.call(cbind, test_list$L)
  
  colnames(right_ear) = gsub("^.*\\.","", colnames(right_ear))
  colnames(left_ear) = gsub("^.*\\.","", colnames(left_ear))
  
  
  # compute PTA
  
  get_thres = function(data,freq,ear,test){return(select(data, contains(paste0(test,"_",ear,"_",freq))))}
  
  pta_L <- (get_thres(data,500,left,"AC") + get_thres(data,1000, left,"AC") + get_thres(data,2000, left,"AC") + get_thres(data,4000, left,"AC"))/4
  pta_R <- (get_thres(data,500,right,"AC")+ get_thres(data,1000, right,"AC")+ get_thres(data,2000, right,"AC")+ get_thres(data,4000, right,"AC"))/4
  
  
  right_ = pta_R >= pta_L
  right_ = right_[,1] # decide based on AC
  
  ear <- c()
  
  for (r in 1:length(right_)){
    
    # if worse ear not given, use ear with more data, else left is default
    if (is.na(right_[r])){
      if (sum(is.na(left_ear[r,])) > sum(is.na(right_ear[r,])))
        ear <- rbind(ear, as.numeric(right_ear[r,]))
      else{
        ear <- rbind(ear, as.numeric(left_ear[r,]))
      }
    }else if (right_[r] == TRUE){
      ear <- rbind(ear, as.numeric(right_ear[r,]))
    } else if (right_[r] == FALSE){
      ear <- rbind(ear, as.numeric(left_ear[r,]))
    }
  } # rowloop
  
  ear_names = colnames(right_ear)
  ear_names = gsub('R_', '', ear_names)
  
  colnames(ear) = ear_names
  ear <- as.data.frame(ear)
  
  ear$ASYM = abs(unlist(pta_R[,1], use.names = FALSE) - unlist(pta_L[,1], use.names = FALSE))
  
  
  if ( sum(is.na(test_betterWorse)) == 0){
    
    
    df = c()
    for (t in test_betterWorse){
      tmpdf=  reformat_to_better_worse_ear(data, worse_right = right_, test = t)
      
      df = cbind(df, tmpdf)
      
    }
    
    names_notRL = grep("R|L", colnames(data), ignore.case = FALSE, value = TRUE, invert= TRUE)
    
    
    single_ear_data <- cbind(ear, df)
    
    notRL_data =  select(data, contains(names_notRL))
    
    data = cbind(notRL_data, single_ear_data)
    
  } else if (sum(is.na(test_betterWorse)) != 0){
    
    
    single_ear_data = ear
    
    notRL_data = select(data, contains(names_notRL))
    
    data = cbind(notRL_data, single_ear_data)
    
  }
  
  return(data)
  
  
}
