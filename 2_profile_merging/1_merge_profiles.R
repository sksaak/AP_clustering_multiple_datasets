################################################################################
#
# Merge profiles generated from two separate datasets 
#
# use either the count data or the fully anonymized data (depending on necessity
# for training classification models)
#
################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())

# libraries
library(ggplot2)
library(ggsci)
library(overlapping)
library(dplyr)


# set up folders and directories
dir.create(file.path("data/2_profile_merging/loop_merge"), recursive = TRUE)  


# functions---------------
source("functions/%not_in%.R")
source("data/variable_limits.R")
source("functions/get_profile_overlap.R")
source("functions/calc_overlap_density.R")
source("functions/compare_feature_similarity.R")

# LOAD DATASET A ---------------------------------------------------------------
load("data/1_profile_generation/A_anonymized_profiles.Rdata")
profilesA = anonymized_profiles
profilesA = lapply(profilesA, function(x) cbind(x, pid = paste0(x[1,"profile"], "_A")))


#load("data/1_profile_generation/A_count_profiles.Rdata")
#datA = count_profiles

# LOAD DATASET B ---------------------------------------------------------------
load("data/1_profile_generation/B_anonymized_profiles.Rdata")
profilesB = anonymized_profiles
profilesB = lapply(profilesB, function(x) cbind(x, pid = paste0(x[1,"profile"], "_B"))) 

#load("data/1_profile_generation/B_count_profiles.Rdata")
#datB = count_profiles

# BALANCE FEATURES (here goesa for merging) ------------------------------------

profilesA = lapply(profilesA, function(x) cbind(x,
                                        goesa_S0N0_bin2 = x$goesa_S0N0_bin,
                                        goesa_S0N0_bin3 = x$goesa_S0N0_bin,
                                        goesa_S0N0_bin4 = x$goesa_S0N0_bin,
                                        goesa_S0N0_bin5 = x$goesa_S0N0_bin,
                                        goesa_S0N0_bin6 = x$goesa_S0N0_bin))

profilesB = lapply(profilesB, function(x) cbind(x,
                                      goesa_S0N0_bin2 = x$goesa_S0N0_bin,
                                      goesa_S0N0_bin3 = x$goesa_S0N0_bin,
                                      goesa_S0N0_bin4 = x$goesa_S0N0_bin,
                                      goesa_S0N0_bin5 = x$goesa_S0N0_bin,
                                      goesa_S0N0_bin6 = x$goesa_S0N0_bin))

# FEATURES USED FOR MERGING ----------------------------------------------------
features = c("ASYM","ABG","UCL_PTA","AC_PTA","BC_PTA","scaled_bisgaard",
             "age",
             "goesa_S0N0_bin","goesa_S0N0_bin2","goesa_S0N0_bin3","goesa_S0N0_bin4","goesa_S0N0_bin5","goesa_S0N0_bin6",
             "acalos_1000_diff","acalos_1000_L15","acalos_1000_L35",
             "acalos_4000_diff","acalos_4000_L15","acalos_4000_L35")   


# JOIN PROFILE DATASETS --------------------------------------------------------

both_profiles = c(lapply(profilesA, function(x) x[,features]),
                  lapply(profilesB, function(x) x[,features]))
merge_profiles = both_profiles

merge_profiles_pid =  c(lapply(profilesA, function(x) x[,c(features, "pid")]),
                        lapply(profilesB, function(x) x[,c(features, "pid")]))

# START MERGING ----------------------------------------------------------------


  for (i in 1:length(both_profiles)-1){

    # calculate overlap 
    merge_profiles_overlap = get_profile_overlap(merge_profiles, features,  savefile = "data/2_profile_merging/loop_merge/merge_sim_", limits = limits)
    merge_profiles_overlap = merge_profiles_overlap[order(merge_profiles_overlap$mean_overlap, decreasing = TRUE),] # highest overlap at first row 
    m = merge_profiles_overlap[1,]
    
    # Merge profiles 
    merge_profiles[[m$c1]] =rbind(merge_profiles[[m$c1]], merge_profiles[[m$c2]])  
    merge_profiles[[m$c2]] = NULL
    
    # merge profiles with pid
    merge_profiles_pid[[m$c1]] =rbind(merge_profiles_pid[[m$c1]], merge_profiles_pid[[m$c2]])  
    merge_profiles_pid[[m$c2]] = NULL
    
    # save merging iteration data and profile set   
    save(m,merge_profiles, merge_profiles_overlap, merge_profiles_pid, file = paste0("data/2_profile_merging/merge_iteration_",i,".Rdata"))
      
      
    } 


# general info for later indexing
info = data.frame(profiles = length(both_profiles),
                  merges = length(both_profiles)-1,
                  cadd = length(both_profiles)+1)

save(info, file = "data/2_profile_merging/merge_info.Rdata")

