################################################################################
#
#  Save profile information anonymously for data sharing
#
#  -> profile information is transformed and shuffled 
#
#  -> shuffling can be done for completely anonymizing data
#  -> if shuffled, classification models cannot yet be run again
#
################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())


# source functions & variable limits
source("data/variable_limits.R")
source("functions/normedCount.R")
source("functions/reformat_clusters_to_counts.R")

# load data
load("data/1_profile_generation/final_mc.Rdata")
load("data/1_profile_generation/profiles.Rdata")

# adjust to name of the respective dataset
dataName = "A" # "B"

# FEATURES TO BE TRANSFORMED ---------------------------------------------------
feats = c("ASYM","ABG","UCL_PTA","AC_PTA","BC_PTA",
          "age",
          "goesa_S0N0_bin","goesa_S0N90_bin","goesa_S0N90_mon","goesa_ILD","goesa_BILD",
          "acalos_1000_L15","acalos_1000_L35","acalos_2000_L15","acalos_2000_L35",
          "acalos_4000_L15","acalos_4000_L35","acalos_1000_diff","acalos_2000_diff","acalos_4000_diff")


# STEP 1: REFORM CLUSTERS TO COUNTS --------------------------------------------
count_profiles = reformat_clusters_to_counts(profiles, feats, limits)


# STEP 2: SHUFFLE DATA FOR COMPLETE ANONYMIZATION (for data sharing) -----------

anonymized_profiles = lapply(count_profiles, function(x)  x[sample(1:nrow(x)), ])


# SAVE DATA --------------------------------------------------------------------

# count profiles to build classification models (if no anonymization is needed)
save(count_profiles, file = paste0("data/1_profile_generation/", dataName, "_count_profiles.Rdata"))

# anonymized profiles if sensitive data should be shared 
save(anonymized_profiles, file = paste0("data/1_profile_generation/", dataName, "_anonymized_profiles.Rdata"))





