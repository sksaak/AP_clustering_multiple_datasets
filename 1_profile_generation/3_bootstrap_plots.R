################################################################################
#
#  Merging bootstrap clusters
#
#
################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())

# libraries
if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

# functions
source("functions/getmode.R")

set.seed(22)

k = c()
cov = c()
sim = c()

for (b in 1:1000){ # 1000 bootstrap iterations

load(paste0("data/1_profile_generation/bootstrap_result/famd_boot",b,".Rdata"))

  k = rbind(k, mod$num_k)
  cov = rbind(cov, as.character(mod$cov))
  
  
}

df <- data.frame(k = as.factor(k), cov = cov)

# Number of profiles
ggplot(data = df, aes(x = k) )+
  geom_bar(stat = "count", fill="#2D708EFF")+
  theme_bw()+
  theme(text = element_text(size = 9))+
  xlab("Number of Profiles")+
  ylab("Frequency")

# Covariance parametrization
ggplot(data = df, aes(x = cov))+
  geom_bar(stat = "count")+
  theme_bw()+
  theme(text = element_text(size = 17))+
  xlab("Covariance")+
  ylab("Frequency")


final_cov = getmode(cov)
final_k = getmode(k)

# save results
save(final_cov, final_k, file= "./data/1_profile_generation/bootstrap_results.Rdata")
  

