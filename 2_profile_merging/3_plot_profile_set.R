################################################################################
#
# Plot final profile set
#
# -> adjust according to features available within the dataset
#
################################################################################

# clear console, objects and plots 
cat("\014")  
rm(list = ls())

# libraries
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)
library(geomtextpath)
library(grid)

# functions

source("functions/merge_lists.R")
source("functions/order_profiles_by_test.R")
source("functions/plot_profile_boxplot.R")
source("functions/plot_profile_polar.R")
source("functions/minMaxScale.R")
source("functions/get_legend.R")


# GET DATA ---------------------------------------------------------------------

load("data/2_profile_merging/pset.Rdata")
load(paste0("data/2_profile_merging/merge_iteration_",pset,".Rdata"))

# reorder data by audiological measure (SRT)
res = order_profiles_by_test(merge_profiles_pid, test = "goesa_S0N0_bin")
df = res$data
profiles = res$profiles
save(res, file = paste0("data/2_profile_merging/profile_ordered_",pset,".Rdata"))

# for plot ordering
df$mergeProfile = factor(df$mergeProfile, levels = length(profiles):1)

# load merge info 
load("data/2_profile_merging/merge_info.Rdata")


# PLOTTING PARAMETERS ----------------------------------------------------------
text_size = 9
yticklabels = as.factor(length(merge_profiles):1)

colfill_AG = "#E2F0D9"
colfill_ACA = "#FFF2CD"
colfill_SRT = "#DDEAF6"
colfill_Anamnesis = "#F2F2F2"

coltitle_AG = "#1D4114"
coltitle_ACA = "#C0870E"
coltitle_SRT = "#3681C0"
coltitle_Anamnesis = "#827B6F" 

# plotting parameters
colorSpeech = c("#DEEAF6","#004FA1")
colorAG = c("#E2F0D9","#223E04")
colorACALOS = c("#FFF2CD","#AC6E01")
colorDemo = c("#F2F2F2","#5C565A")

color_measurements_dark = c(rep(colorAG[2], times = 4),colorDemo[2], rep(colorACALOS[2], times =6), colorSpeech[2])
color_measurements_light = c(rep(colorAG[1], times = 4),colorDemo[1], rep(colorACALOS[1], times =6), colorSpeech[1])


# BOXPLOTS -------------------------------------------------------------------

  # GOESA
  p_goesa_S0N90_bin = plot_profile_boxplot(df = df, profile = df$mergeProfile, feature= df$goesa_S0N0_bin, titlename = "GOESA S0N0 bin", xlabel = "SRT [dB SNR]",
                                 ylabel = "Profiles", xlimits = c(-10,25), yticklabels = yticklabels, text_size, backgroundcolor = colfill_SRT)+
                                 annotate("text", label="Speech",x=Inf, y=Inf, vjust=-1, hjust=1, color = coltitle_SRT, fontface = 2)
  
  # AGE
  p_age = plot_profile_boxplot(df = df, profile = df$mergeProfile, feature= df$age, titlename = "Age", xlabel = "Age [years]",
                               ylabel = "Profiles", xlimits = c(0,100),  yticklabels = yticklabels, text_size, backgroundcolor = colfill_Anamnesis)+
                               annotate("text", label="Anamnesis",x=Inf, y=Inf, vjust=-1, hjust=1, color = coltitle_Anamnesis, fontface = 2)
  
  # AC PTA
  p_AC_PTA = plot_profile_boxplot(df = df, profile = df$mergeProfile, feature= df$AC_PTA, titlename = "AC PTA", xlabel = "Threshold [dB HL]", 
                                   ylabel= "Profiles",xlimits = c(-10,120), yticklabels = yticklabels, text_size,backgroundcolor = colfill_AG)
  # AG ASYM
  p_ASYM = plot_profile_boxplot(df = df, profile = df$mergeProfile, feature= df$ASYM,titlename = "ASYM", xlabel = "Asymmetry [dB]",
                                   ylabel = "Profiles",xlimits = c(-10,120), yticklabels = yticklabels, text_size, backgroundcolor = colfill_AG)
  # AG ABG
  p_ABG = plot_profile_boxplot(df = df, profile = df$mergeProfile, feature= df$ABG,titlename = "ABG", xlabel = "ABG [dB]",
                                   ylabel = "Profiles",xlimits = c(-10,120), yticklabels = yticklabels, text_size, backgroundcolor = colfill_AG)+
                                   annotate("text", label="Audiogram",x=Inf, y=Inf, vjust=-1, hjust=1, color = coltitle_AG, fontface = 2) # change this to the plot that should carry the audiogram annotation
  
  # AG UCL PTA
  p_UCL_PTA = plot_profile_boxplot(df = df, profile = df$mergeProfile, feature= df$UCL_PTA, titlename = "UCL PTA", xlabel = "UCL PTA [dB HL]",
                                   ylabel = "Profiles", xlimits = c(45,125),yticklabels = yticklabels, text_size, backgroundcolor = colfill_AG)
  
  
  # ACALOS
  aca1000 = data.frame(L15 = as.numeric(lapply(profiles, function(x) mean(x$acalos_1000_L15) )),
                       L35 = as.numeric(lapply(profiles, function(x) mean(x$acalos_1000_L35) )),
                       y15 = 15,
                       y35 = 35, 
                       profile = factor(names(profiles), levels = length(profiles):1))
  
  
  aca4000 = data.frame(L15 = as.numeric(lapply(profiles, function(x) mean(x$acalos_4000_L15) )),
                       L35 = as.numeric(lapply(profiles, function(x) mean(x$acalos_4000_L35) )),
                       y15 = 15,
                       y35 = 35, 
                       profile =  factor(names(profiles), levels = length(profiles):1))
  
  # beware - shapes cannot handle large profiles numbers
  p_acalos_1000 = ggplot(data = aca1000)+
    geom_hline(yintercept = 15, linetype = "dashed", color = "grey")+
    geom_hline(yintercept = 35, linetype = "dashed", color = "grey")+
    geom_point(aes(x = L15, y = y15, group = profile, color = profile, shape = profile), size = 3)+
    geom_point(aes(x = L35, y = y35, group = profile, color = profile, shape = profile), size = 3)+
    geom_label(x = 54, y = 48, label = "- NH ref.")+ 
    theme_bw()+
    theme(plot.background = element_rect(fill = colfill_ACA), 
          legend.position = "bottom", 
          legend.text = element_text(size = 10, face = 2),
          legend.title = element_text(size = 10, face = 2),
          text = element_text(size = text_size),
          plot.title = element_text(size = text_size))+
    scale_color_viridis_d(name = "Profile")+
    scale_shape_manual(name = "Profile", values = c(1:length(profiles)))+
    guides(color = guide_legend(nrow = 2), 
           shape = guide_legend(nrow = 2))+
    ylab("Loudness [CU]")+
    xlab("Level [dB]")+
    coord_cartesian(ylim = c(0,50), xlim = c(45,120), expand = FALSE)+
    ggtitle("ACALOS 1 kHz")
  
  # beware - shapes cannot handle large profiles numbers
  p_acalos_4000 = ggplot(data = aca4000) +
    geom_hline(yintercept = 15, linetype = "dashed", color = "grey")+
    geom_hline(yintercept = 35, linetype = "dashed", color = "grey")+
    geom_point(aes(x = L15, y = y15, group = profile, color = profile, shape = profile), size = 3)+
    geom_point(aes(x = L35, y = y35, group = profile, color = profile, shape = profile), size = 3)+
    geom_label(x = 54, y = 48, label = "- NH ref.")+ 
    theme_bw()+
    theme(plot.background = element_rect(fill = colfill_ACA), 
          legend.position = "none", 
          text = element_text(size = text_size),
          plot.title = element_text(size = text_size))+
    scale_color_viridis_d()+
    scale_shape_manual(values = c(1:length(profiles)))+
    ylab("Loudness [CU]")+
    xlab("Level [dB]")+
    coord_cartesian(ylim = c(0,50), xlim = c(45,120), expand = FALSE)+
    ggtitle("ACALOS 4 kHz")
  
  
  p_acalos_1000_diff = plot_profile_boxplot(df = df, profile = df$mergeProfile, feature= df$acalos_1000_diff, titlename = "ACALOS 1 kHz ", xlabel = "L35-L15 [dB]",
                                              ylabel = "Profiles", xlimits = c(-5,75), yticklabels = yticklabels, text_size, backgroundcolor = colfill_ACA) +
                                              annotate("text", label="Loudness Scaling",x=Inf, y=Inf, vjust=-1, hjust=1, color = coltitle_ACA, fontface = 2) 
  
  p_acalos_4000_diff = plot_profile_boxplot(df = df, profile = df$mergeProfile, feature= df$acalos_1000_diff, titlename = "ACALOS 4 kHz ",xlabel = "L35-L15 [dB]",
                                              ylabel = "Profiles", xlimits = c(-5,75), yticklabels = yticklabels, text_size, backgroundcolor = colfill_ACA)
  
  # get shape legend
  shapelegend= get_legend(p_acalos_1000)

# POLAR PLOTS ------------------------------------------------------------------
    
    # in order as later factor ordering is desired
    features =  c("AC_PTA","ABG","ASYM","UCL_PTA",
                  "age",
                  "acalos_1000_L15","acalos_1000_L35","acalos_1000_diff",
                  "acalos_4000_L15","acalos_4000_L35","acalos_4000_diff",
                  "goesa_S0N0_bin") 
    featureGroup = c(rep("Audiogram", times = 4), "Anamnesis", rep("Loudness Scaling", times = 6), "Speech test")
    
    # define the curved labels - depend on the number of features per feature group - adjust according to included features
    labelAG = data.frame(x = seq(from=0.5, to=4.5, by=0.5), y = 1, z = "Audiogram")   
    labelSpeech = data.frame(x = seq(from=11.5, to=12.5, by=0.5), y = 1, z = "Speech test")   
    labelACALOS = data.frame(x = seq(from=5.5, to=11.5, by=0.5), y = 1, z = "Loudness Scaling")   
    labelAnam = data.frame(x = seq(from=4.5, to=5.5, by=0.5),y = 1, z = "Anamnesis")  
    
    # Normal hearing reference for scaling (NH Profile of merged profile set) (minMax scaling) 
    # -> needs to be arranged in order of features
    # describes the reference for the datasets A & B of Saak et al., (2024) 
    # for dataset comparability minMaxScaling needs to include predefined minMax scores that remain the same across datasets 
    ref = c(0.09473684, 0.02898551, 0.01162791, 0.56603774, 0.32575758,
            0.34090909, 0.68918919, 0.25531915, 0.34444444, 0.67532468, 0.29787234, 0.11764706)
    names(ref) = features
    
    
    polar_list = list()
    
    for (p in as.character(1:length(profiles))){
    
    polar_list[[p]] = plot_profile_polar(df, features, featureGroup, profileColumn = "mergeProfile", profileIDX = p, 
                       changeFeatureDirection =c("acalos_1000_diff", "acalos_4000_diff"), ref = ref )
    
    }
    # get the legend as a separate plot
    polar_list[[1]] = polar_list[[1]] + 
      theme(legend.position = "bottom")+ 
      guides(fill = guide_colourbar(barwidth = 10, barheight = 0.5, direction = "horizontal"))
   
    polarlegend= get_legend(polar_list[[1]])
    
    
# FINAL PLOT -------------------------------------------------------------------

    p_boxplots = ggarrange(
    
      p_goesa_S0N90_bin, p_age,
      p_AC_PTA,p_ABG,
      p_UCL_PTA,p_ASYM,
      p_acalos_1000+ theme(legend.position = "none"),p_acalos_1000_diff,
      p_acalos_4000,p_acalos_4000_diff, 
      nrow = 5, ncol = 2
      
    )
    
    # select polar plots by hand (list)
    p_polar =  ggarrange( 
      # ROW 1
      polar_list[[1]] +
        theme(legend.position = "none"), 
      # ROW 2
      polar_list[[3]] +
        theme(legend.position = "none"),
      # ROW 3
      polar_list[[8]] +
        theme(legend.position = "none"),
      # ROW 4
      ggplot()+ 
        geom_blank()+
        theme_void()+
        annotate("text",label = "...", x = 0, y = 0, fontface = 2, color = "black", size = 15),
      # ROW 5 
      polar_list[[13]] +
        theme(legend.position = "none"), 
      
      nrow = 5, ncol = 1) + bgcolor("white")
    
    p_legends =  ggarrange(shapelegend, polarlegend, widths = c(2,1)) + bgcolor("white")
  
 # boxplots + selected polar plots    
    p_all= ggarrange(
      ggarrange(p_boxplots, p_polar, labels = c("A","B"), ncol = 2, widths = c(2,1)),
      p_legends,
      ncol = 1, nrow = 2,
      heights = c(5,0.3)
    )
    
    
 # only boxplots
    p_boxplots = ggarrange(p_boxplots, shapelegend, nrow = 2, heights = c(5,0.2))
 # only polar plots   
    p_allpolar = ggarrange(plotlist = polar_list, common.legend = TRUE, ncol = 3, nrow = 5)


    
# SAMPLE SIZE PLOT 
    
    groups = data.frame(values = df$group,
                        profile = as.factor(df$mergeProfile))
    
    groups_mat = as.data.frame(matrix(data = 0, nrow=length(merge_clusters_id), ncol = info$clusters))
    ind = as.data.frame(matrix(data=NA, ncol=length(merge_clusters_id), nrow = info$clusters))
    values = as.data.frame(matrix(data=NA, ncol=length(merge_clusters_id), nrow = info$clusters))
    
    for (p in 1:length(merge_clusters_id)){
      out_length = length(names(table(groups$values[groups$profile == p])))
      ind[1:out_length,p] = names(table(groups$values[groups$profile == p]))
      values[1:out_length,p] = as.numeric(table(groups$values[groups$profile == p]))
    }
    
    ind2 =   stack(ind)
    values2 = stack(values)
    
    groups = data.frame(profile = as.numeric(ind2$ind), 
                        original = ind2$values,
                        n = values2$values)
    groups = groups[complete.cases(groups),]
    groups$profile = factor(groups$profile, levels = length(merge_clusters_id):1)
    dataset = matrix(data = NA,ncol = 1, nrow = 1:nrow(ind2))
    dataset[grep("HZO",groups$original)] = "A"
    dataset[grep("HDG",groups$original)] = "B"
    groups$dataset = dataset
    
    groups$original = gsub("HDG_", "", groups$original)
    groups$original = gsub("HZO_", "", groups$original)
    
    
    p_groups32 = ggplot(groups, aes(y= profile, x= n,fill = dataset))+
      geom_col(color = "black") +
      #geom_text(aes(label=original), position = position_stack(vjust = 0.5), color="black", size=2.5)+
      theme_bw()+
      theme(text = element_text(size = text_size-1))+
      scale_y_discrete("Profiles", labels = profile_order$new)+
      scale_x_continuous(name = "Sample size", n.breaks = 8)+
      scale_fill_discrete(name = "Dataset")+
      xlab("Sample size")
    
    p_groups20= ggplot(groups, aes(y= profile, x= n,fill = dataset))+
      geom_col(color = "black") +
      #geom_text(aes(label=original), position = position_stack(vjust = 0.5), color="black", size=2.5)+
      theme_bw()+
      theme(text = element_text(size = text_size-1))+
      scale_y_discrete("Profiles", labels = profile_order$new)+
      scale_x_continuous(name = "Sample size", n.breaks = 8)+
      scale_fill_discrete(name = "Dataset")+
      xlab("Sample size")  
    
    
# PROFILE MERGE PLOT (SAMPLE SIZE AND DATA GROUP) ------------------------------
    
  # get new (merged) profile and original profile index
    groups = data.frame(values = df$pid,
                        profile = as.factor(df$mergeProfile))
    
  # initialize vars
    groups_mat = as.data.frame(matrix(data = 0, nrow=length(merge_profiles_pid), ncol = info$profiles))
    ind = as.data.frame(matrix(data=NA, ncol=length(merge_profiles_pid), nrow = info$profiles))
    values = as.data.frame(matrix(data=NA, ncol=length(merge_profiles_pid), nrow = info$profiles))
    
  # extract sample size of groups which groups were merged   
    for (p in 1:length(merge_profiles_pid)){
      out_length = length(names(table(groups$values[groups$profile == p])))
      ind[1:out_length,p] = names(table(groups$values[groups$profile == p]))
      values[1:out_length,p] = as.numeric(table(groups$values[groups$profile == p]))
    }
    
    ind2 =   stack(ind)
    values2 = stack(values)
    
    # make dataset 
    groups = data.frame(profile = as.numeric(ind2$ind), 
                        original = ind2$values,
                        n = values2$values)
    groups = groups[complete.cases(groups),]
    groups$profile = factor(groups$profile, levels = length(merge_profiles_pid):1)
    dataset = matrix(data = NA,ncol = 1, nrow = 1:nrow(ind2))
    dataset[grep("A",groups$original)] = "A"
    dataset[grep("B",groups$original)] = "B"
    groups$dataset = dataset
    
    p_sampleSizeGroups = ggplot(groups, aes(y= profile, x= n,fill = dataset))+
      geom_col(color = "black") +
      #geom_text(aes(label=original), position = position_stack(vjust = 0.5), color="black", size=2.5)+
      theme_bw()+
      theme(text = element_text(size = text_size-1))+
      scale_y_discrete("Profiles", labels = res$profile_order$new)+
      scale_x_continuous(name = "Sample size", n.breaks = 8)+
      scale_fill_discrete(name = "Dataset")+
      xlab("Sample size")

    
    
    # SAVE PLOTS -------------------------------------------------------------------
    ggsave("plots/2_profile_merging/boxplot_polar.png", p_all, units = "px", width = 3000, height = 4950)#width = 5330, height = 3000)
    ggsave("plots/2_profile_merging/boxplots.png", p_boxplots, units = "px", width = 3000, height = 4950)#width = 5330, height = 3000)
    ggsave("plots/2_profile_merging/polar_plots.png", p_allpolar, units = "px", width = 3000, height = 4950)#width = 5330, height = 3000)
    ggsave("plots/2_profile_merging/sampleSizeGroup.png", plot=p_sampleSizeGroups, units = "cm", width = 17,height = 7, dpi = 300)
    

    
    
    
    