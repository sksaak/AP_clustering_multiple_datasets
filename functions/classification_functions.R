# FUNCTIONS USED FOR CLASSIFICATION

merge_lists <- function(cluster){
  # rbind all dataframes in lists to one dataframe 
  
  dat <- data.frame()
  for (c in 1:length(cluster)){
    
    profile = rep(c, times = nrow(cluster[[c]]))
    dat <- rbind(dat, cbind(cluster[[c]], profile))
  }
  return(dat)
}
get_metric <- function(conf, positive, negative){
  # Get sensitivity, specificity, precision, and f1 score
  #
  # conf = confusion matrix
  # positive = name of positive 
  # negative = name of negative
  

  TP = conf[positive,positive] 
  TN = conf[negative,negative]
  FP = conf[positive,negative]
  FN = conf[negative,positive]
  
  sens = TP/(TP + FN) 
  spec = TN/(TN+FP)
  prec = TP/(TP+FP)
  f1 = 2*TP/(2*TP + FP + FN)
  
  train_metric = data.frame(sens = sens, 
                            spec = spec, 
                            prec = prec, 
                            f1 = f1)

  return(train_metric)
}



logmat <- function(n) {
  # logical matrix to extract all miss-labels
  diag(n)==0
  }

# UPSAMPLING
balance_classes <- function(df, method, levels = c(1,0), min_n = 60){
  # balances the dataset of minority/majority cases with respect to min_n, i.e., 
  # the minority data is upsampled to min_n
  #
  # df = dataframe with two classes 
  # method = upsample or upsample with noise
  # levels = how the classes are indexed, defaults to c(1,0)
  # min_n = minority data is upsampled to min_n, default is 60
  
  
  minority = df[df$profile == levels[1],]
  majority = df[df$profile == levels[2],]
  
  if (method == "upsample"){
    idx = sample(1:nrow(minority), size = nrow(majority), replace = TRUE)
    
    new_cases = minority[idx, ]
    
    balanced = rbind(majority, new_cases)
    
  } else if(method == "upsample_with_noise"){
    
    new_data <- upsample_with_gaussian_noise(df, df$profile, min_n = min_n)
    
    balanced = new_data
    
  } else{print("error")}
  
  return(balanced)
}
upsample_with_gaussian_noise <- function(df, class, min_n){
  # Upsampling with gaussian noise 
  #
  # df = dataframe with two classes 
  # class = profile labels 
  # min_n = minority data is upsampled to min_n
  
  class_to_upsample <- names(which(table(class) < min_n))
  new_data = NULL
  
  for (c in class_to_upsample){
    
    selected_data <- df[which(class == c),]
    n_more <- min_n-nrow(selected_data)
    
    tmp_sample <- selected_data[sample(nrow(selected_data), n_more, replace = TRUE), ]
    tmp_new_data = tmp_sample
    
    
    for (cols in 1:ncol(tmp_sample)){
      
      if(class(tmp_sample[,cols]) == "numeric"){
        col_sd <- sd(selected_data[,cols])
        sd_range <- seq(from = -col_sd, to = col_sd, by = (2*col_sd)/100 )
        noise <- sample(x = sd_range, size = nrow(tmp_sample), replace = TRUE)
        tmp_new_data[,cols] <- tmp_sample[,cols] + noise
        
      } 
    }
    
    new_data <- rbind(new_data, tmp_new_data)
    
  }
  
  
  return(new_data)
  
}

# tuneGrids
mtryGrid = function(train_data){
rfGrid =  expand.grid(mtry = c(1:(ncol(train_data)-1)))
return(rfGrid)
}

