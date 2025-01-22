################################################################################
#         General Preprocessing of a dataset for profile generation                   
#
#         - reading in data
#         - adhering to naming conventions 
#         - feature generation 
#         
################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())

# set working directory to project directory
# > Session > Set Working Directory > To Project Directory  

# set up folders and directories
dir.create(file.path("data/1_profile_generation/"), recursive = TRUE)  

# libraries 
library(readxl)
library(dplyr)
library(ggplot2)
library(pracma)


# source functions
source("functions/ag_interpolate.r")
source("functions/ag_set_max.r")
source("functions/%not_in%.r")
source("functions/select_worse_ear_from_measurements.R")
source("functions/get_bisgaard.R")
source("functions/order_bisgaard_by_PTA.R")


set.seed(22) 


# LOAD DATA --------------------------------------------------------------------

# < YOUR CODE HERE > 

# -> name data.frame  "dat"

# load your dataset
load("data/dat.R")


# ADJUST FEATURE NAMES ACCORDING TO THE NAMING CONVENTION (see naming_conventions.md) -----

# < YOUR CODE HERE > 

# FEATURE IMPUTATION FOR THE AUDIOGRAM -----------------------------------------
# If feasible, missing values are interpolated 

for(row in 1:nrow(dat)){

  #ACR
  dat[row,grepl("AC_R", colnames(dat))] <-  ag_interpolate(dat[row, grepl("AC_R", colnames(dat))], max = 120)
  #ACL
  dat[row,grepl("AC_L", colnames(dat))] <-  ag_interpolate(dat[row,grepl("AC_L", colnames(dat))], max = 120)
  #BCR
  dat[row,grepl("BC_R", colnames(dat))] <-  ag_interpolate(dat[row,grepl("BC_R", colnames(dat))], max = 120, impute_upper = FALSE)
  #BCL
  dat[row,grepl("BC_L", colnames(dat))] <-  ag_interpolate(dat[row,grepl("BC_L", colnames(dat))], max = 120, impute_upper =FALSE)
}  

# CHECK MISSING DATA -----------------------------------------------------------
# check for columns with all NA values & delete
all.nan <- sapply(dat, function(x)all((is.na(x)))) 
deleted.vars <- list()
deleted.vars$all.nan <- list(names(dat[all.nan]))
dat[all.nan] <- NULL

# count NaNs for each column and kick out vars that have more than 40% missing variables
nan.column <-sapply(dat, function(y) sum(length(which(is.na(y)))))
nan.column.frame <-data.frame(nan.column)

# count proportion missing for each variable
propMiss <- (colSums(  is.na(dat) )/nrow(dat))
propMiss.list <- list()
propMiss.list$vars$all <- list(propMiss)
propMiss.list$vars$ten <- list(propMiss[propMiss < 0.1])
propMiss.list$vars$twenty <- list(propMiss[propMiss > 0.1 & propMiss< 0.2])
propMiss.list$vars$thirty <- list(propMiss[propMiss > 0.2 & propMiss< 0.3])
propMiss.list$vars$forty <- list(propMiss[propMiss > 0.3 & propMiss<0.4])
propMiss.list$vars$more <- list(propMiss[propMiss > 0.4])


# KICK OUT VARIABLES WITH MORE THAN 40% MISSING --------------------------------
# !!!! not by default to but requires checking !!!!
#
# <YOUR CODE HERE>


# SELECT WORSE EAR FROM MEASUREMENT --------------------------------------------
data = select_worse_ear_from_measurements(dat, tests = c("AC", "BC", "UCL", "acalos"), test_betterWorse = c("oto", "val", "tymp"))


# FEATURE GENERATION -----------------------------------------------------------
# comment out code for features not available 
# add features that are of interest


# calculate PTA of UCL (uncomfortable loudness) (normal: 90-110 dB)
data$UCL_PTA = (data$UCL_500 + data$UCL_1000 + data$UCL_2000 + data$UCL_4000)/4

# calculate PTA of BC 
data$BC_PTA = (data$BC_500 + data$BC_1000 + data$BC_2000 + data$BC_4000)/4

# PTA
data$AC_PTA = (data$AC_500 + data$AC_1000 + data$AC_2000 + data$AC_4000)/4

## compute ABG 
data$ABG =  data$AC_PTA - data$BC_PTA

# compute ILD & BILD
data$goesa_ILD = data$goesa_S0N0_bin - data$goesa_S0N90_bin
data$goesa_BILD = data$goesa_S0N90_mon - data$goesa_S0N90_bin

# compute BISGAARD class

bis_freqs = c("AC_250", "AC_500", "AC_750", "AC_1000", "AC_1500", "AC_2000", "AC_3000", "AC_4000", "AC_6000")


# get bisgaard profile index for each patient 
data$bisgaard = get_bisgaard(data, bis_freqs)

# order bisgaard by PTA 
data$scaled_bisgaard = order_bisgaard_by_PTA(data$bisgaard) 


# select the final feature set used for clustering and store in data! 
# <Your CODE> 


# check that factors are also stored as factors
# <YOUR CODE>


# REMOVE SUBJECTS THAT HAVE INFORMATION ON ONLY ONE AUDIOLOGICAL MEASURE 
# Example code: remove subjects with only GOESA, ACALOS, or AG
# -> adjust to the respective available features
# data_to_boot = get_at_least_2_measures_patients(data)



save(data, file = "data/1_profile_generation/preprocessed_data.Rdata")



