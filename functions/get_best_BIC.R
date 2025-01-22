
get_best_BIC <- function(BIC){
  # Get best model from BIC object
  # 
  # BIC = BIC object
  # 
  # returns (model) = model with number of clusters, covariance index, BIC value
  # 

  
    
    mod = summary(BIC)[1]
    
    mod_ = names(mod)
    mod_ <- strsplit(mod_, ",")
    cov = as.character(unlist(lapply(mod_, "[[", 1)))
    num_k = as.numeric(unlist(lapply(mod_, "[[", 2)))
    
    model <- data.frame(cov = cov, num_k = num_k, mod = mod)
    

  return(model)
}