dummy_classifier = function(p,feat,seed = FALSE){
  
  
  if (is.numeric(seed)){set.seed(seed)}
  
  load(paste0("data/3_classification/train_parameters/",feat,"_train_params_",p,".Rdata"))
  # functions
  source("functions/classification_functions.R")
  i_mat <- logmat(2)
  
  profile_n = length(levels(train_data$profile))
  # LOOP ACROSS PROFILES -------------------------------------------------------
  for (ap in 1:profile_n){
    
    # AP1
    ap_dat <- train_data
    ap_dat$profile <- as.numeric(ap_dat$profile)
    ap_dat$profile[ap_dat$profile != ap] = "neg"
    ap_dat$profile[ap_dat$profile == ap] = "pos"
    ap_dat$profile <- factor(ap_dat$profile)
    
    # shuffle rows
    ap_dat= ap_dat[sample(1:nrow(ap_dat)), ]
    
    # get probability weights for stratified sampling
    N = sum(table(ap_dat$profile))
    plabel = names(table(ap_dat$profile))
    probA = as.vector(table(ap_dat$profile))[1]/N
    probB = as.vector(table(ap_dat$profile))[2]/N
    probs = c(probA, probB)

    # random forest model 
    train_res = data.frame(true = ap_dat$profile,
                           pred = sample(x = plabel, prob = probs, size = N, replace = TRUE))
    conf = table(train_res)
    miss = conf[i_mat]

    # train prediction ---------------------------------------------- 
    train_metric = get_metric(conf, positive = "pos", negative = "neg")
    
    # val prediction ------------------------------------------------
    val_data_ap = val_data
    val_data_ap$profile <- as.numeric(val_data_ap$profile)
    val_data_ap$profile[val_data_ap$profile != ap] = "neg"
    val_data_ap$profile[val_data_ap$profile == ap] = "pos"
    val_data_ap$profile <- factor(val_data_ap$profile)
    
    N = sum(table(val_data_ap$profile))
    plabel = names(table(val_data_ap$profile))
    probA = as.vector(table(val_data_ap$profile))[1]/N
    probB = as.vector(table(val_data_ap$profile))[2]/N
    probs = c(probA, probB)
    
    val_res = data.frame(true = val_data_ap$profile,
                           pred = as.factor(sample(x = plabel, prob = probs, size = N, replace = TRUE)))
    
    tmp <- caret::confusionMatrix(val_res$pred, reference = val_data_ap$profile)
    conf = tmp$table
    val_metric = get_metric(conf, positive = "pos", negative = "neg")
    
    # test prediction -----------------------------------------------
    test_data_ap = test_data
    test_data_ap$profile <- as.numeric(test_data_ap$profile)
    test_data_ap$profile[test_data_ap$profile != ap] = "neg"
    test_data_ap$profile[test_data_ap$profile == ap] = "pos"
    test_data_ap$profile <- factor(test_data_ap$profile)
    
    N = sum(table(test_data_ap$profile))
    plabel = names(table(test_data_ap$profile))
    probA = as.vector(table(test_data_ap$profile))[1]/N
    probB = as.vector(table(test_data_ap$profile))[2]/N
    probs = c(probA, probB)
    
    test_res = data.frame(true = test_data_ap$profile,
                         pred = as.factor(sample(x = plabel, prob = probs, size = N, replace = TRUE)))
    
    tmp <- caret::confusionMatrix(test_res$pred , reference = test_data_ap$profile)
    conf = tmp$table
    test_metric = get_metric(conf, positive = "pos", negative = "neg")
    
    metrics <- rbind(train_metric, val_metric, test_metric)
    metrics$cond = c("train","val", "test")
    
    print(paste("AP:", ap,  "    Metric:", trainMet ))
    print(conf)
    
    # set up folders and directories
    dir.create(file.path(paste0("data/3_classification/dummy/",feat,"/")), recursive = TRUE, showWarnings = FALSE)  
    
    save(metrics, file = paste0("data/3_classification/dummy/",feat,"/",feat,"_pset_",p,"_Metrics_",
                                trainMet,"_",ap,"_.Rdata"))
    # delete for storage
    rm(metrics)
  }
  
  
  
  
}
  
  