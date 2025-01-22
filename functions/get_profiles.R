get_profiles <- function(mc, data){
  # Returns a list of clusters, given the mc object and the data 
  #
  #
  
  cluster = c()
  for (c in 1:max(mc$classification)){
    
    dat <- data[(mc$classification ==  c & mc$uncertainty < 0.4),]
    cluster[[c]] <- dat #dat[c(1:4,10:61)]
  }
  return(cluster)
}