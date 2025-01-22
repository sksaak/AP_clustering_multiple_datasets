get_featImp_ova = function(feat, p, np){
  # Extracts the feature importance from the ova_train function result
  # 
  # feat = name of the feature set used, e.g. "ACALOS"
  # p = profile set
  # np = number of profiles in profile set 
  
  
  
  # get train metric
  featImp = c()
  for (ap in 1:np){
    load(paste0("data/3_classification/ova/",feat,"/",feat, "_pset_", p, "_Model_Kappa_", ap,"_.Rdata"))
    
    feature = as.data.frame(t(model$finalModel$importance)  )
    feature$ap = ap
    feature$cond = feat
    # feature$cond = cond
    featImp = rbind(featImp, feature)
  }
  
  return(featImp)
}
