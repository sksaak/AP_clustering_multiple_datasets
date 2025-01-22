predict_ova = function(data, m, feat, cv, p, np){
  # Predict ova or ovaovo for data
  #
  # data = train_data, test_data, val_data
  # m = "kappa"
  # feat = feature group, e.g. ACALOS
  # cv = cross validation name
  # p = pset, index of profile set
  # np = number of profiles in profile set
  
  preds = c()
  out_probs = list()
  for (ap in 1:np){
    load(paste0("data/3_classification/ova/",feat,"/", feat, "_pset_",p, "_Model_", m, "_", ap,"_.Rdata"))
    
    out = predict(model, data)
    out_prob = predict(model, data, "prob")
    out = as.character(out)
    out[out == "neg"] = 0
    out[out == "pos"] = ap
    
    preds = cbind(preds, as.numeric(out))
    out_probs[[ap]] = out_prob
    
  }
  colnames(preds) = c(1:np) 
  
  # loop over each row, and store indices with more than one prediction or no prediction -------
  res_mat = matrix(data = NA, nrow = nrow(preds), ncol = 1)
  idx = c()
  aps = list()
  for (r in 1:nrow(preds)){
    out = table(preds[r,])
    if (length(out) == 1 && names(out) == "0"){ # no prediction made
      
      pos_pred = do.call("rbind",lapply(out_probs, function(x) x[r,2])) 
      pos_pred = which(pos_pred == max(pos_pred))
      
      if (length(pos_pred) > 1){pos_pred =  sample(pos_pred, size = 1)}
      
      res_mat[r] = pos_pred
      
    } else if(length(out) == 2){ # one prediction made
      res_mat[r] = getmin(out)
    }else if (length(out) > 2){ # multiple predictions made
      
      idx = c(idx,r)
      outn = names(out)
      aps[[r]] = as.numeric(outn[names(out) != "0"])
      
    }
  }
  
  out = list(pred = res_mat, 
             idx = idx, 
             aps = aps)
  

    # get prediction with highest probability-------
    if (is.null(out$idx)){
    } else if ( !is.null(out$idx)){
      new_pred = c()
      for (r in 1:length(out$idx)){
        
        potentialAPS = aps[[out$idx[r]]]
        
        probs = c()
        for (ps in potentialAPS){
          prob = out_probs[[ps]][idx[r],2]  
          probs = c(prob, probs)
        }
        
        new_ap = potentialAPS[which(probs == max(probs))]
        if (length(new_ap > 1 )){new_ap =  sample(new_ap, size = 1)}
        
        new_pred = c(new_pred, new_ap )
        
      }
      out[["new_pred"]] = new_pred
      
      res_mat[out$idx] = out$new_pred
      
    }
    
    
    res_mat = factor(res_mat, levels = 1:np)
    
    AUPRC = caret::confusionMatrix(res_mat, data$profile)
    
    res = list(metric =AUPRC$byClass[,c(1,2,5,7)],
               pred = res_mat)
    
    return(res)
  
}