reformat_clusters_to_counts = function(clusters, names, limits){
  # reformats clusters/profiles to count data given specified limits
  #
  # clusters = list of clusters/profiles
  # names = vector of names of features to be transformed 
  # limits = list of min/max limits of the features to be transformed 
  #
  # returns transformed cluster list
  
  
  cluster_count = clusters
  for (c in 1:length(clusters)){
    
    clust = clusters[[c]]
    
    for (name in names){
      # reformat to count data 
      
      # get 100 equidistant steps from min limit to max limit 
      out =  normedCount(as.numeric(clust[,name]), from = limits[[name]][1], to = limits[[name]][2])
      breaks = out$mids
      
      old =  as.numeric(clust[,name])
      new = old # initialize counts 
      
      for (i in 1:length(old)){
        
        dist = old[i] - breaks # calculate the distance to each break point to select the closest one
        
        new[i] =  breaks[which(abs(dist) == min(abs(dist)))][1] # ensures that the first of double mins is used 
        
      }
      
      clust[,name] = new
    }
    
    cluster_count[[c]]  = clust
  }
  
  return(cluster_count)
  
}
