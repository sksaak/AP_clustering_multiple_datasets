ova_train = function(p,feat, seed = FALSE){
  
# libraries
if (!require(randomForest)) install.packages('randomForest')
if(!require(caret)) install.packages('caret')
library(randomForest)
library(caret)

set.seed(22)

# functions
source("functions/classification_functions.R")

# load data ----------------
load(paste0("data/3_classification/train_parameters/",feat,"_train_params_",p,".Rdata"))

# general vars
i_mat <- logmat(2)
rfGrid = mtryGrid(train_data)


profile_n = length(levels(train_data$profile))
# LOOP ACROSS PROFILES -------------------------------------------------------
for (ap in 1:profile_n){
  
  # AP1
  ap_dat <- train_data
  ap_dat$profile <- as.numeric(ap_dat$profile)
  ap_dat$profile[ap_dat$profile != ap] = "neg"
  ap_dat$profile[ap_dat$profile == ap] = "pos"
  ap_dat$profile <- factor(ap_dat$profile)
  
  if (is.numeric(seed)){set.seed(seed)}
  new_data = balance_classes(ap_dat, method = "upsample_with_noise", levels = c("pos","neg"), min_n = min_n)

  ap_dat = rbind(ap_dat, new_data)

  # shuffle rows
  ap_dat= ap_dat[sample(1:nrow(ap_dat)), ]

  model_weights <- ifelse(ap_dat$profile == "neg",
                          (1/table(ap_dat$profile)[1]) * 0.5,
                          (1/table(ap_dat$profile)[2]) * 0.5)

    # random forest model 
    model<- train(profile ~ ., data = ap_dat, 
                            method = "rf", 
                            trControl = fitControls,
                            ntree = c(500),
                            weights = model_weights,
                            tuneGrid = rfGrid,
                            metric = trainMet, 
                            verbose = TRUE
    )


    # train prediction ---------------------------------------------- 
    conf <- model$finalModel$confusion
    miss <- conf[i_mat]
    train_metric = get_metric(conf, positive = "pos", negative = "neg")
    
    # val prediction ------------------------------------------------
    val_data_ap = val_data
    val_data_ap$profile <- as.numeric(val_data_ap$profile)
    val_data_ap$profile[val_data_ap$profile != ap] = "neg"
    val_data_ap$profile[val_data_ap$profile == ap] = "pos"
    val_data_ap$profile <- factor(val_data_ap$profile)
    
    pred <- predict(model, newdata=val_data_ap)
    tmp <- caret::confusionMatrix(pred, reference = val_data_ap$profile)
    conf = tmp$table
    val_metric = get_metric(conf, positive = "pos", negative = "neg")
    
    # test prediction -----------------------------------------------
    test_data_ap = test_data
    test_data_ap$profile <- as.numeric(test_data_ap$profile)
    test_data_ap$profile[test_data_ap$profile != ap] = "neg"
    test_data_ap$profile[test_data_ap$profile == ap] = "pos"
    test_data_ap$profile <- factor(test_data_ap$profile)
    
    pred <- predict(model, newdata=test_data_ap)
    tmp <- caret::confusionMatrix(pred, reference = test_data_ap$profile)
    conf = tmp$table
    test_metric = get_metric(conf, positive = "pos", negative = "neg")

    metrics <- rbind(train_metric, val_metric, test_metric)
    metrics$cond = c("train","val", "test")
    
    
    print(paste("AP:", ap,  "    Metric:", trainMet ))
    print(conf)
    
    # set up folders and directories
    dir.create(file.path(paste0("data/3_classification/ova/",feat,"/")), recursive = TRUE, showWarnings = FALSE)  
    
    save(model, file = paste0("data/3_classification/ova/",feat,"/",feat,"_pset_",p,"_Model_",
                                trainMet,"_",ap, "_.Rdata"))
    
    save(metrics, file = paste0("data/3_classification/ova/",feat,"/",feat,"_pset_",p,"_Metrics_",
                                trainMet,"_",ap,"_.Rdata"))
    # delete for storage
    rm(model, metrics)
    }
 
} 