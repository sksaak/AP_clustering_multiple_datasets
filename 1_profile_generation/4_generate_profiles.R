################################################################################
#
#  Generate the profile set for the respective dataset
#
#
################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())
dev.off()

# libraries
if (!require(missMDA)) install.packages('missMDA')
if (!require(mclust)) install.packages('mclust')
if(!require(tidyverse)) install.packages('tidyverse')
library(missMDA)
library(mclust)
library(tidyverse)

set.seed(22)

#functions
source("functions/minMaxScale.R")
source("functions/get_profiles.R")

set.seed(22)

# LOAD --------------------------------------------------------------------
load("data/1_profile_generation/preprocessed_data.Rdata")
load("data/1_profile_generation/bootstrap_results.Rdata")

# IMPUTATION -------------------------------------------------------------------
cat_var = names(Filter(is.factor, data))

# IMPUTATION WITH FAMD -------------------------------------------------------
famd.res = missMDA::imputeFAMD(data)

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
             "acalos_2000_L15", "acalos_2000_L35", "acalos_2000_diff",
             "acalos_4000_L15", "acalos_4000_L35", "acalos_4000_diff", 
             "AC_PTA", "BC_PTA", "ASYM", "ABG","UCL_PTA", "scaled_bisgaard" )        


data_cluster = famd.data[,features]

# MODEL BASED CLUSTERING -------------------------------------------------------
scaled_dat = minMaxScale(data_cluster)

mc = Mclust(scaled_dat, G = final_k, modelNames = final_cov)
summary(mc)


save(mc,data_cluster, file = paste0("data/1_profile_generation/final_mc.Rdata"))


# GENERATE PROFILE LIST --------------------------------------------------------

profiles = get_profiles(mc, data_cluster)

save(profiles, file="data/1_profile_generation/profiles.Rdata")


