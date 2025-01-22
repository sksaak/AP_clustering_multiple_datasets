calc_average_mets = function(df, trainTestVal, feats){
  #  Calculates the average metrics for a specific data condition (train, test, val)
  # 
  # df = data.frame
  # trainTestVal = df$data condition of either train, test, or val, 
  # feats = features used 

  
  
  val_mets = df[df$data == trainTestVal,]
  mets_df = matrix(data = NA, nrow = length(levels(as.factor(val_mets$cond))), ncol = length( levels(val_mets$metric)))
  colnames(mets_df) = levels(val_mets$metric)
  rownames(mets_df) = levels(as.factor(val_mets$cond))
  
  for (feat in  feats){
    tmp = c()
    for (cond in levels(val_mets$metric)){
      tmp = c(tmp,  mean(val_mets[val_mets$metric == cond & val_mets$cond == feat, "values"], na.rm = TRUE))
    }
    mets_df[feat,] = tmp
  }
  mets_df = as.data.frame(mets_df)
  mets_df = stack(mets_df)
  mets_df$cond = rep( levels(as.factor(val_mets$cond)), times = length( levels(val_mets$metric)))
  
  mets_df$cond = factor(mets_df$cond, levels = feats)
  

  out = as.data.frame(mets_df)
  
  return(out)
}