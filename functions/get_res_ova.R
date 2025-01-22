get_res_ova = function(feat, p, cond, np){ 
  # get results from the ova classification
  #
  # feat = feature
  # p = profile set
  # cond = train, test, val
  # np = number of profiles in the set 
  
  mets = c()
  trainMet = c("Kappa")
  for (m in trainMet){
    # get train metric
    met = c()
    for (ap in 1:np){
      load(paste0("./data/3_classification/ova/",feat,"/",feat, "_pset_", p, "_Metrics_", m, "_", ap,"_.Rdata"))
      
      metric = metrics[metrics$cond == cond,]
      metric$metric = m
      metric$ap = ap
      met = rbind(met, metric)
    }
    
    mets = rbind(mets, met)
    
  }
  
  return(mets)
}