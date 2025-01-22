get_res_dummy = function(feat, p, m){
  all_metrics = c()
  for (ap in 1:np){
    load(paste0("data/3_classification/dummy/",feat,"/", feat, "_pset_",p, "_Metrics_", m, "_", ap,"_.Rdata"))
    
    metrics$ap = ap
    all_metrics = rbind(all_metrics, metrics) 
    
  }
  
  return(all_metrics)
}
