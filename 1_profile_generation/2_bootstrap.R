################################################################################
#
#  Bootstrapping for profile generation to learn 
#  the optimal profile number and covariance matrix      
#
#
################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())

# libraries
library(factoextra)
library(mclust)
library(dplyr)
library(missMDA)
set.seed(22)

# set up folders and directories
dir.create(file.path("data/1_profile_generation/bootstrap_result"), recursive = TRUE)  

#functions
source("functions/%not_in%.R")
source("functions/bootstrap.R")
source("functions/minMaxScale.R")
source("functions/get_best_BIC.R")

set.seed(22)

# load data used for clustering
load("data/preprocessed_data.Rdata")

# PARAMETERS -------------------------------------------------------------------
text_size = 17
bootstart = 1
boot = 1000

# generate seeds for reproducible bootstrapping------------
if (file.exists("data/seeds.Rdata")){
  load("data/seeds.Rdata")
} else {
  # set reproducible seeds
  seeds <- sample(1:10000, 1000, replace = FALSE)
  save(seeds, file= "data/seeds.Rdata")
}

#---BOOTSTRAP ------------------------------------------------------------------
for (b in c(bootstart:boot)){

  set.seed(seeds[b])
  df<- bootstrap(data, "subsample",perc = 0.9)
  cat_var = names(Filter(is.factor, df))
  
  # IMPUTATION WITH FAMD -------------------------------------------------------
  famd.res = missMDA::imputeFAMD(df, method = "Regularized")

  #  MCA FOR CATEGORICAL FEATURE ENCODING  -------------------------------------
  pcs = 3 # number of retained components -> adjust according to dataset
  famd.mca.res = FactoMineR::MCA(famd.res$completeObs[,cat_var], ncp = length(cat_var), graph = FALSE, method = "Burt")
  famd.data = cbind(famd.res$completeObs[, names(famd.res$completeObs) %not in% cat_var],famd.mca.res$ind$coord[,1:pcs]) 
  
  
  # CORRECT FOR IMPUTATION MISTAKES (e.g. round bisgaard, correct ILD & BILD) ----
  # adjust to included features
  famd.data$ASYM[famd.data$ASYM < 0] = 0
  famd.data$BC_PTA[famd.data$BC_PTA > famd.data$AC_PTA] = famd.data$AC_PTA[famd.data$BC_PTA > famd.data$AC_PTA]
  famd.data$ABG = famd.data$AC_PTA - famd.data$BC_PTA
  famd.data$ILD = famd.data$goesa_S0N0_bin - famd.data$goesa_S0N90_bin
  famd.data$BILD = famd.data$goesa_S0N90_mon - famd.data$goesa_S0N90_bin
  
  famd.data$acalos_1000_L15[famd.data$acalos_1000_L15 > 120] = 120
  famd.data$acalos_2000_L15[famd.data$acalos_2000_L15 > 120] = 120
  famd.data$acalos_4000_L15[famd.data$acalos_4000_L15 > 120] = 120
  famd.data$acalos_1000_L35[famd.data$acalos_1000_L35 > 120] = 120
  famd.data$acalos_2000_L35[famd.data$acalos_2000_L35 > 120] = 120
  famd.data$acalos_4000_L35[famd.data$acalos_4000_L35 > 120] = 120
  famd.data$acalos_diff_1000 = famd.data$acalos_1000_L35 - famd.data$acalos_1000_L15
  famd.data$acalos_diff_2000 = famd.data$acalos_2000_L35 - famd.data$acalos_2000_L15
  famd.data$acalos_diff_4000 = famd.data$acalos_4000_L35 - famd.data$acalos_4000_L15
  
  # GENERATE FEATURE SET USED FOR CLUSTERING -----------------------------------
  # adjust to own feature set 
  features = c("age",
               "goesa_S0N0_bin","goesa_S0N90_bin","goesa_S0N90_mon","goesa_ILD", "goesa_BILD",
               "acalos_1000_L15", "acalos_1000_L35", "acalos_1000_diff", 
               "acalos_4000_L15", "acalos_4000_L35", "acalos_4000_diff", 
               "AC_PTA", "BC_PTA", "ASYM", "ABG","UCL_PTA", "scaled_bisgaard" )        

  
  data_cluster = famd.data[,features]
  
  
  
  # GET OPTIMAL K ACROSS COMPLETED DATASETS ------------------------------------
  dat <- minMaxScale(data_cluster)
  
  set.seed(22)
  BIC <- mclustBIC(dat, G = 2:40, modelNames = c("EII", "VII", "EEI", "VEI"), verbose = FALSE)
  mod <- get_best_BIC(BIC)
  
  
  result = data.frame(k = mod$num_k)
  
  save(result, mod, BIC, file = paste0("data/1_profile_generation/bootstrap_result/famd_boot", b, ".Rdata"))
  print(paste("Boot:", b, " K:", result$k))
}




