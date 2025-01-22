get_profile_overlap = function(all_clusters, savefile = FALSE, both_feat, limits){
  # Header function for calculating the overlap between two cluster lists
  #
  # cluster1 = clusters 
  # savefile = determines if and how the data is saved
  # both_feat = features used for merging
  # limits = variable limits, used for overlapping density calculations
  
  
  savefile = savefile
  overlap = list()
  df <- data.frame()
  
  # all possible combination
  combs = c()
  for (c1 in 1:length(all_clusters)){
    for (c2 in 1:length(all_clusters)){
      
      if (c1 != c2 ){ # only calculate if not the same cluster
        
        if (paste(c2,c1) %not in% combs){ # check if combinations was calculated before 
          
          
          df= compare_feature_similarity(all_clusters, c1,c2, df = df, limits)
          
          
          combs = c(combs, paste(df$c1, df$c2))
          
          if (savefile != FALSE ){
            save(df, file= paste0(savefile, c1, "_",c2, ".Rdata"))
          }
          
          
          print(paste(c1,c2))
        }
      }
    }
  }
  
  
  # remove double entries and self profiles 
  overlap = df
  overlap$mean_overlap = rowMeans(df[,both_feat])
  
  return(overlap)
}