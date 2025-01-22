reformat_to_better_worse_ear = function(data, worse_right, test){ 
  # reformat Otoskopie, valsalva, Tympanogram to better and worse ear 
  # data = dataset that contains columns with the test names 
  # worse_right = defaults to worse right ear in code 
  # test = single test e.g."oto", "val", "tymp"
  
  
  better_ear = c()
  worse_ear = c()
  
  for (r in 1:length(worse_right)){
    
    if (is.na(worse_right[r]) | worse_right[r] == FALSE){
      better_ear = rbind(better_ear,c(data[r,paste0(test, "_R")]))  
      worse_ear = rbind(worse_ear,c(data[r,paste0(test, "_L")]))  
    } else if (worse_right[r] == TRUE){
      worse_ear = rbind(worse_ear, c(data[r,paste0(test, "_R")]))  
      better_ear= rbind(better_ear,c(data[r,paste0(test, "_L")]))  
    }
  }
  
  df = cbind(worse_ear, better_ear)
  colnames(df) = c(paste0(test, "_worse"), paste0(test, "_better"))
  
  
  return(df)
  
}
