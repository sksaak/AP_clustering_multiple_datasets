ovo_train = function(p,feat, seed = FALSE){

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

# make profile numeric for indexing
train_data$profile = as.numeric(train_data$profile)
val_data$profile = as.numeric(val_data$profile)
test_data$profile = as.numeric(test_data$profile)

# general vars
i_mat <- logmat(2)

profile_n = length(unique(train_data$profile))


# make model grid --------------------------------------------------------------
a = c()
b = c()

ps = c(1:profile_n)
a = rep(c(1:profile_n), each = profile_n-1)

for (p in 1:profile_n){b = c(b, ps[-p])  }

apGrid = data.frame(a = a, b = b)      

rfGrid = mtryGrid(train_data)


# LOOP ACROSS comparison models ------------------------------------------------
for (r in 1:nrow(apGrid)){
  
  ap1 = apGrid[r,1]
  ap2 = apGrid[r,2]
  
  dat = rbind(train_data[train_data$profile == ap1,], train_data[train_data$profile == ap2,])
  
  dat$profile <- as.numeric(dat$profile)  
  dat$profile[dat$profile == ap1] = paste0("class_",ap1)
  dat$profile[dat$profile == ap2] = paste0("class_",ap2)
  
  if (is.numeric(seed)){set.seed(seed)}
  new_data = balance_classes(dat, method = "upsample_with_noise", levels = c("pos","neg"), min_n = 100)

  ap_dat = rbind(dat, new_data)

  # shuffle rows
  ap_dat= ap_dat[sample(1:nrow(ap_dat)), ]

  # LOOP across different model parameterization

    
    model<- train(profile ~ ., data = dat, 
                  method = "rf", 
                  trControl = fitControls,
                  ntree = 500,
                  tuneGrid = rfGrid,
                  metric = trainMet, 
                  verbose = TRUE
    )
    
    
    # train prediction ---------------------------------------------- 
    conf <- model$finalModel$confusion[1:2,1:2]
    miss <- conf[i_mat]
    train_metric = get_metric(conf, positive = paste0("class_",ap1), negative = paste0("class_",ap2))
    train_metric$ap = paste(ap1,ap2)
    # val prediction ------------------------------------------------
    
    dat = rbind(val_data[val_data$profile == ap1,], val_data[val_data$profile == ap2,])
    
    dat$profile <- as.numeric(dat$profile)  
    dat$profile[dat$profile == ap1] = paste0("class_",ap1)
    dat$profile[dat$profile == ap2] = paste0("class_",ap2)
    dat$profile = as.factor(dat$profile)
    
    pred <- predict(model, newdata=dat)
    
    tmp <- caret::confusionMatrix(pred, reference = dat$profile)
    conf = tmp$table
    tmp <- tmp[["byClass"]]
    
    val_metric = data.frame(ap = paste(ap1,ap2),
                             sens = tmp[1],
                             spec = tmp[2],
                             prec = tmp[5],
                             f1   = tmp[7])
    
    # test prediction ----------------------------------------------
    dat = rbind(test_data[test_data$profile == ap1,], test_data[test_data$profile == ap2,])
    
    dat$profile <- as.numeric(dat$profile)  
    dat$profile[dat$profile == ap1] = paste0("class_",ap1)
    dat$profile[dat$profile == ap2] = paste0("class_",ap2)
    dat$profile = as.factor(dat$profile)
    
    pred <- predict(model, newdata=dat)
    
    tmp <- caret::confusionMatrix(pred, reference = dat$profile)
    conf = tmp$table
    tmp <- tmp[["byClass"]]
    
    test_metric = data.frame(ap = paste(ap1,ap2),
                             sens = tmp[1],
                             spec = tmp[2],
                             prec = tmp[5],
                             f1   = tmp[7])
    
    
    metrics <- rbind(train_metric, val_metric, test_metric)
    metrics$cond = c("train","val","test")
    print(paste("AP:", ap1, "vs.",ap2, "    Metric:", trainMet ))
    print(conf)
    
  
  
    # set up folders and directories
    dir.create(file.path(paste0("data/3_classification/ovo/",feat,"/")), recursive = TRUE, showWarnings = FALSE)  
    
    save(model, file = paste0("data/3_classification/ovo/",feat,"/",feat,"_pset_",p,"_Model_", trainMet,"_",ap1, "_", ap2,".Rdata"))
    save(metrics, file = paste0("data/3_classification/ovo/",feat,"/",feat,"_pset_",p,"_Metric_", trainMet,"_",ap1, "_", ap2,".Rdata"))
    # delete for storage
    rm(model, metrics)
  }
}

