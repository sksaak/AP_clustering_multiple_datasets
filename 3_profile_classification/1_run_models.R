#
# Build classification models with Random forest
#
# OVAOVO - Model
# Kappa (RepCV)
#
####################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())

# set up folders and directories
dir.create(file.path("data/3_classification/train_parameters"), recursive = TRUE)  

# run scripts
source("functions/parameters_for_classification.R")
source("functions/ova_train.R")
source("functions/ovo_train.R")
source("functions/dummy_classifier.R")

# LOAD DATA --------------------------------------------------------------------
load("data/2_profile_merging/merge_info.Rdata")
load("data/2_profile_merging/pset.Rdata")

# classification feature groups 
feat_cond = c("ALL","APP", "HA", "AG", "AG_GOESA", "AG_ACALOS","GOESA", "GOESA_ACALOS", "ACALOS")

p = pset

# GENERATE TRAINING PARAMETERS -------------------------------------------------
# adjust according to desired classification models 

load(paste0("data/2_profile_merging/profile_ordered_",p,".Rdata"))
  for ( feat in feat_cond){
    # set parameters for classification
    set_parameters_for_classification(data=res$data,p =p, feat= feat ) 
  }


# TRAIN MODELS -----------------------------------------------------------------
  for (feat in feat_cond){ 
    
    # run models 
    ova_train(p, feat = feat, seed = 22)
    dummy_classifier(p, feat, seed = 22)
  }

# finished model run
print("ALL MODEL TRAINING FINISHED")


 

