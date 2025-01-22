################################################################################
#
#  Plot clustering results
#
#
################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())

library(ggplot2)
library(dplyr)
library(ggpubr)


# functions
source("functions/plot_boxplot.R")

# set up folders and directories
dir.create(file.path("plots/1_profile_generation/profiles"), recursive = TRUE)  

# load data
load("data/1_profile_generation/final_mc.Rdata")
dat = data_cluster
dat$profile = mc$classification

# Plotting parameters
text_size = 17

# MEAN GOESA S0N0 FOR PROFILE ORDERING -----------------------------------------
mean_goesa = data.frame(srt = unlist(lapply(profiles, function(x) { mean(x$goesa_S0N0_bin, na.rm = TRUE)})),
                      profile = c(1:length(profiles)))
mean_goesa = mean_goesa[order(mean_goesa[,1]),]

dat$profile = ordered(dat$profile, levels = mean_goesa[,2])


# AUDIOGRAM PLOTS --------------- ----------------------------------------------

p_AC_PTA  = plot_boxplot(dat, y="profile", x="AC_PTA", fill="profile", xlims = c(-20,120), xlabel = "PTA [dB HL]", title = "AC PTA" )
p_BC_PTA  = plot_boxplot(dat, y="profile", x="BC_PTA", fill="profile", xlims = c(-20,120), xlabel = "PTA [dB HL]", title = "BC PTA" )
p_UCL_PTA = plot_boxplot(dat, y="profile", x="UCL_PTA", fill="profile", xlims = c(-20,120), xlabel = "PTA [dB HL]", title = "UCL PTA" )
p_ABG     = plot_boxplot(dat, y="profile", x="ABG", fill="profile", xlims = c(0,120), xlabel = "PTA [dB]", title = "ABG" )
p_ASYM    = plot_boxplot(dat, y="profile", x="ASYM", fill="profile", xlims = c(0,120), xlabel = "PTA [dB]", title = "ASYM" )

p_audiogram = ggarrange(p_AC_PTA, p_BC_PTA, p_ABG,
          p_ASYM, p_UCL_PTA, ncol = 3, nrow = 2)


# GOESA PLOTS ------------------------------------------------------------------

p_goesa_S0N0_bin  = plot_boxplot(data = dat, y="profile", x="goesa_S0N0_bin", fill="profile", xlims = c(-15,22), xlabel = "SNR [dB]", title = "GOESA S0N0 bin")
p_goesa_S0N90_bin = plot_boxplot(data = dat, y="profile", x="goesa_S0N90_bin", fill="profile", xlims = c(-15,22), xlabel = "SNR [dB]", title = "GOESA S0N90 bin")
p_goesa_S0N90_mon = plot_boxplot(data = dat, y="profile", x="goesa_S0N90_mon", fill="profile", xlims = c(-15,22), xlabel = "SNR [dB]", title = "GOESA S0N90 mon")
p_goesa_ILD       = plot_boxplot(data = dat, y="profile", x="goesa_ILD", fill="profile", xlims = c(-15,22), xlabel = "SNR [dB]", title = "GOESA ILD")
p_goesa_BILD      = plot_boxplot(data = dat, y="profile", x="goesa_BILD", fill="profile", xlims = c(-15,22), xlabel = "SNR [dB]", title = "GOESA BILD")

p_goesa = ggarrange(p_goesa_S0N0_bin, p_goesa_S0N90_bin, p_goesa_S0N90_mon,
          p_goesa_ILD, p_goesa_BILD, ncol = 3, nrow = 2)

# ACALOS PLOTS -----------------------------------------------------------------

p_acalos_1000_L15  = plot_boxplot(data = dat, y="profile", x="acalos_1000_L15", fill="profile", xlims = c(0,120), xlabel = "Level [dB]", title = "ACALOS 1000 Hz L15")
p_acalos_1000_L35  = plot_boxplot(data = dat, y="profile", x="acalos_1000_L35", fill="profile", xlims = c(0,120), xlabel = "Level [dB]", title = "ACALOS 1000 Hz L35")
p_acalos_1000_diff = plot_boxplot(data = dat, y="profile", x="acalos_1000_diff",fill="profile", xlims = c(0,120), xlabel = "L35-L15 [dB]", title = "ACALOS 1000 Hz diff")

p_acalos_2000_L15  = plot_boxplot(data = dat, y="profile", x="acalos_2000_L15", fill="profile", xlims = c(0,120), xlabel = "Level [dB]", title = "ACALOS 2000 Hz L15")
p_acalos_2000_L35  = plot_boxplot(data = dat, y="profile", x="acalos_2000_L35", fill="profile", xlims = c(0,120), xlabel = "Level [dB]", title = "ACALOS 2000 Hz L35")
p_acalos_2000_diff = plot_boxplot(data = dat, y="profile", x="acalos_2000_diff",fill="profile", xlims = c(0,120), xlabel = "L35-L15 [dB]", title = "ACALOS 2000 Hz diff")


p_acalos_4000_L15  = plot_boxplot(data = dat, y="profile", x="acalos_4000_L15", fill="profile", xlims = c(0,120), xlabel = "Level [dB]", title = "ACALOS 4000 Hz L15")
p_acalos_4000_L35  = plot_boxplot(data = dat, y="profile", x="acalos_4000_L35", fill="profile", xlims = c(0,120), xlabel = "Level [dB]", title = "ACALOS 4000 Hz L35")
p_acalos_4000_diff = plot_boxplot(data = dat, y="profile", x="acalos_4000_diff",fill="profile", xlims = c(0,120), xlabel = "L35-L15 [dB]", title = "ACALOS 4000 Hz diff")

p_acalos = ggarrange(p_acalos_1000_L15, p_acalos_1000_L35, p_acalos_1000_diff,
          p_acalos_2000_L15, p_acalos_2000_L35, p_acalos_2000_diff,
          p_acalos_4000_L15, p_acalos_4000_L35, p_acalos_4000_diff, 
          ncol = 3, nrow = 3)


# save plots -------------------------------------------------------------------

ggsave("plots/1_profile_generation/profiles/audiogram.tiff", p_audiogram)
ggsave("plots/1_profile_generation/profiles/goesa.tiff", p_goesa)
ggsave("plots/1_profile_generation/profiles/acalos.tiff", p_acalos)


# save profile info ------------------------------------------------------------
save(profiles, file= "data/1_profile_generation/profiles.Rdata")




