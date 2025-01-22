
set_parameters_for_classification = function(data,p,feat){
  
  # libraries
  if (!require(rlist)) install.packages('rlist')
  if (!require(randomForest)) install.packages('randomForest')
  if(!require(caret)) install.packages('caret')
  if(!require(caTools)) install.packages('caTools')
  library(rlist)
  library(randomForest)
  library(caret)
  library(caTools)
  
  
  # SEEDS for repeated 10-fold CV
  set.seed(22)
  seeds <- vector(mode = "list", length = 101)
  for(i in 1:100) seeds[[i]] <- sample.int(1000, 14) 
  ## For the last model:
  seeds[[101]] <- sample.int(1000, 1)
  
  
  # FITCONTROLS -----------------------------------------------------------------
  fitControls = list()
  
  #kappa
  fitControls <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 10,
                              verboseIter = TRUE,
                              seeds = seeds
  )
  
  trainMet = "Kappa"
  
  # make feature groups ----------------------------------------------------------
  
  feats = list()
  
  # single
  feats$AG = c("profile", "AC_PTA", "ASYM", "scaled_bisgaard", "age")
  feats$GOESA = c("profile", "goesa_S0N0_bin", "age")
  feats$ACALOS =  c("profile", "age",
                    "acalos_4000_L15", "acalos_4000_L35", "acalos_4000_diff",
                    "acalos_1000_L15", "acalos_1000_L35", "acalos_1000_diff")
 
  # combined
  feats$AG_GOESA = c("profile", "goesa_S0N0_bin", "AC_PTA","ASYM", "scaled_bisgaard", "age")
  feats$AG_ACALOS = c("profile", "AC_PTA","ASYM", "scaled_bisgaard", "age",
                      "acalos_4000_L15", "acalos_4000_L35", "acalos_4000_diff",
                      "acalos_1000_L15", "acalos_1000_L35", "acalos_1000_diff")
  feats$GOESA_ACALOS = c("profile", "goesa_S0N0_bin", "age",
                       "acalos_4000_L15", "acalos_4000_L35", "acalos_4000_diff",
                       "acalos_1000_L15", "acalos_1000_L35", "acalos_1000_diff")
  # usecase
  feats$ALL = c("profile", "goesa_S0N0_bin", "age",
                "AC_PTA", "ASYM", "scaled_bisgaard", "BC_PTA", "ABG", "UCL_PTA",
                "acalos_4000_L15", "acalos_4000_L35", "acalos_4000_diff",
                "acalos_1000_L15", "acalos_1000_L35", "acalos_1000_diff")
  feats$APP = c("profile", "goesa_S0N0_bin", "age",
                "AC_PTA", "ASYM", "scaled_bisgaard",
                "acalos_4000_L15", "acalos_4000_L35", "acalos_4000_diff",
                "acalos_1000_L15", "acalos_1000_L35", "acalos_1000_diff")
  feats$HA = c("profile", "goesa_S0N0_bin", "age",
                "AC_PTA", "ASYM", "scaled_bisgaard", "BC_PTA", "ABG", "UCL_PTA")
  

  
  df = data
  df$profile <- as.factor(df$mergeProfile)
  
  df <- df[,feats[[feat]]]
  df = df[sample(1:nrow(df)), ] # shuffle the data
  
  
  # train and test set
  set.seed(22)
  tt_set <- caTools::sample.split(df$profile, SplitRatio = 0.8)
  
  alltrain_data <- df[tt_set,]
  test_data = df[!tt_set,]
  
  val_set = caTools::sample.split(alltrain_data$profile, SplitRatio = 0.8)
  
  train_data = alltrain_data[val_set,]
  val_data = alltrain_data[!val_set,]
  
  
  
  table(train_data$profile)
  table(val_data$profile)
  table(test_data$profile)
  
  min_n = round(mean(table(train_data$profile)))
  
  
  save(tt_set,seeds, file = paste0("data/3_classification/train_parameters/",feat,"_tt_set_",p,".Rdata"))
  save(fitControls, trainMet, train_data, val_data, test_data, min_n,
       file =paste0("data/3_classification/train_parameters/",feat,"_train_params_",p,".Rdata"))
  
}

