compare_feature_similarity = function(all_clusters,c1,c2,df, limits){
  # calculate the feature similarity between two profiles across all features
  # 
  # all_clusters = all clusters to be used 
  # c1 = cluster number
  # c2 = cluster number
  # df = data.frame that stores the results 
  
  sim <- c() 
  for (name in names(all_clusters[[c1]])){
    
    cmp1 <- all_clusters[[c1]][,name]
    cmp2 <- all_clusters[[c2]][,name] 
    
    sim[name] <- round(  calc_overlap_density(cmp1,cmp2,limits = c(limits[[name]][1], limits[[name]][2])),  4)
    
    
  }
  
  sim["c1"] <- c1
  sim["c2"] <- c2
  
  df <- rbind(df,as.data.frame(t(sim)))
  
  
  return(df)
} # loop iteration across profiles