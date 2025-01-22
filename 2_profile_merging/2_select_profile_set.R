################################################################################
#
# Select profile set from the merging iterations
#
# -> merging areas need to be selected by hand 
#
################################################################################

# libraries
library(ggplot2)
library(geomtextpath)
library(ggpubr)
library(ggbrace)

# clear console, objects and plots 
cat("\014")  
rm(list = ls())

# set up folders and directories
dir.create(file.path("plots/2_profile_merging/"), recursive = TRUE)  

# Plotting parameters ----------------------------------------------------------
text_size = 9
# colors for categories 
colorSpeech = c("#DEEAF6","#004FA1")
colorAG = c("#E2F0D9","#223E04")
colorACALOS = c("#FFF2CD","#AC6E01")
colorDemo = c("#F2F2F2","#5C565A")

# define the curved labels -----------------------------------------------------
labelAG = data.frame(y = seq(from=7.5, to=13.5, by=0.5),
                     x = -11,
                     z = "Audiogram")   
labelSpeech = data.frame(y = seq(from=13.5, to=14.5, by=0.5),
                         x = -11,
                         z = "Speech test")   
labelACALOS = data.frame(y = seq(from=0.5, to=6.5, by=0.5),
                         x = -11,
                         z = "Loudness Scaling")   
labelAnam = data.frame(y = seq(from=6.5, to=7.5, by=0.5),
                       x = -11,
                       z = "Anamnesis")   
color_measurements_dark = c(rep(colorAG[2], times = 4),colorDemo[2], rep(colorACALOS[2], times =6), colorSpeech[2])
color_measurements_light = c(rep(colorAG[1], times = 4),colorDemo[1], rep(colorACALOS[1], times =6), colorSpeech[1])

feat_names = features = c("ASYM","ABG","UCL_PTA","AC_PTA","BC_PTA","scaled_bisgaard","age",  "goesa_S0N0_bin",
                          "acalos_1000_diff","acalos_1000_L15","acalos_1000_L35",
                          "acalos_4000_diff","acalos_4000_L15","acalos_4000_L35")
feat_names_order = c("goesa_S0N0_bin","AC_PTA","BC_PTA","ABG", "ASYM","UCL_PTA","scaled_bisgaard", "age",
                          "acalos_1000_L15","acalos_1000_L35","acalos_1000_diff",
                          "acalos_4000_L15","acalos_4000_L35","acalos_4000_diff" )
# LOAD INFO --------------------------------------------------------------------
load("data/2_profile_merging/merge_info.Rdata")


# LOOP THROUGH MERGE ITERATIONS ------------------------------------------------
cut_iter = c()
overlap_iter = matrix(data = NA, nrow=2000, ncol = info$merges )
merge = c()
save_plot = FALSE
all_feat = c()
all_feat_combs = c()

# merging areas - can be adjusted given the datasets 
merging_areas = list(A = 32:43, 
                     B = 20:31, 
                     C = 20:43,
                     D = 1:19,
                     E = 1:43)


# merging iteration loop
for (i in 1:info$merges){
  
  load(paste0("data/2_profile_merging/merge_iteration_",i,".Rdata"))
  
  merge_profiles_overlap = merge_profiles_overlap[order(merge_profiles_overlap$mean_overlap, decreasing = TRUE),]
  m = merge_profiles_overlap[1,]
  
    # feature importance of merged profiles 
  feat = merge_profiles_overlap[merge_profiles_overlap$c1 == m$c1 & merge_profiles_overlap$c2 == m$c2,]
  all_feat = rbind(all_feat, feat)
  
  # feature overlap of all possible combinations
  feat_combs = merge_profiles_overlap[,feat_names]
  feat_combs = apply(feat_combs,2,mean)
  
  all_feat_combs = rbind(all_feat_combs, feat_combs)
  
  # average overlap
  cut_iter = c(cut_iter, m$mean_overlap[1])
  overlap_iter[1:nrow(merge_profiles_overlap),i] = merge_profiles_overlap$mean_overlap
  merge=  rbind(merge, as.numeric(m[1,c("c1","c2")]))
  
}


# PLOT 1 HIGHEST & MEAN OVERLAP ACROSS ITERATIONS ------------------------------

# median & highest overlap of all profiles across merging iterations
median_overlap = apply(overlap_iter,2,median, na.rm = TRUE)

df_cutiter = stack(data.frame(highest_cutoff = cut_iter,
                              median_cutoff = median_overlap))
df_cutiter$iteration = rep(1:info$merges, times = 2)

df_highest_overlap = df_cutiter[df_cutiter$ind == "highest_cutoff",]
df_median_overlap = df_cutiter[df_cutiter$ind == "median_overlap",]

# slope
slope1 = round(lm(df_highest_overlap$values[merging_areas$D]~df_highest_overlap$iteration[merging_areas$D])$coefficients[2],4)
slope2 = round(lm(df_highest_overlap$values[merging_areas$B]~df_highest_overlap$iteration[merging_areas$B])$coefficients[2],4)
slope3 = round(lm(df_highest_overlap$values[merging_areas$A]~df_highest_overlap$iteration[merging_areas$A])$coefficients[2],4)
# variance
var1 = round(var(df_median_overlap$values[merging_areas$D], na.rm = TRUE),4)
var2 = round(var(df_median_overlap$values[merging_areas$B]),4)
var3 = round(var(df_median_overlap$values[merging_areas$A]),4)

p_cutoff_iteration = ggplot(data = df_cutiter,aes(x = iteration, y = values, group = ind, color = ind) )+
  geom_line()+
  geom_point()+
  geom_vline(xintercept = 20, color = "green", linetype= "dashed")+
  geom_vline(xintercept = 32, color = "green", linetype= "dashed")+
  annotate("text", x = c(10,26,38,  10,26,38), y = c(0.9,0.9,0.9,  0.1,0.1,0.1), 
           label = c(slope1,slope2,slope3, 
                     format(var1, scientific = FALSE), format(var2, scientific = FALSE),format(var3, scientific = FALSE)),
           color=c("#F8766D","#F8766D", "#F8766D", "#03BFC4", "#03BFC4","#03BFC4"), 
           size=3 , fontface="bold")+
  geom_brace(aes(y = c(0.98,0.945), x=c(min(merging_areas$A), max(merging_areas$E)), label="A"), inherit.data=F, labelsize=3)+
  geom_brace(aes(y = c(0.98,0.945), x=c(min(merging_areas$B), max(merging_areas$E)), label="B"), inherit.data=F, labelsize=3)+
  geom_brace(aes(y = c(0.98,0.945), x=c(min(merging_areas$D), max(merging_areas$E)), label="D"), inherit.data=F, labelsize=3)+
  theme_bw()+
  theme(text = element_text(size = text_size),
        legend.position = "bottom",
        strip.placement = "none",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = c(colorSpeech[1], colorAG[1], colorDemo[1], colorACALOS[1]) ),  # Make facet label background white.
        panel.spacing = unit(0, units = "cm"),
        plot.margin = unit(c(0.1, 0.01, 0.0, 0.165), units="npc"))+
  coord_cartesian(ylim = c(0.1,0.9), xlim=c(0.5,43.5), clip = "off") +
  scale_color_discrete(name = "Overlap", labels = c("highest", "median"))+
  scale_x_continuous(name = "Iteration", expand = c(0.0,0.0))+
  scale_y_continuous("Overlapping density", breaks = seq(from = 0.1, to = 0.9, by=0.1))+
  xlab("Iteration")


# PLOT 2 HEATMAP OF FEATURE IMPORTANCE ACROSS ITERATION ------------------------

all_feat_heat = all_feat[,c(feat_names,"mean_overlap")]
all_feat_heat = stack(all_feat_heat)
all_feat_heat$iteration = 1:nrow(all_feat)

# cluster for plot ordering
all_feat_matrix <- t(as.matrix(all_feat[,c(feat_names,"mean_overlap")]))
rownames(all_feat_matrix) = colnames(all_feat[,c(feat_names,"mean_overlap")])
all_feat_dendro <- as.dendrogram(hclust(d = dist(x = all_feat_matrix)))

all_feat_dendroOrder <- order.dendrogram(all_feat_dendro) # ordered based on dendrogram
all_feat_order = rev(feat_names_order) # adjust order to wishes
all_feat_heat$ind <- factor(x = all_feat_heat$ind,
                            levels = all_feat_order, 
                            ordered = TRUE)


p_mergeFeaturesHeatmap = ggplot(all_feat_heat, aes(y = ind, x = iteration, fill = values))+
  geom_tile() +
  geom_hline(yintercept = 14.5, color = "white")+
  # color rectangulars
  geom_rect(xmin = -11, xmax = 0.5, ymax = 14.5, ymin = 13.5, fill = colorSpeech[1])+
  geom_rect(xmin = -11, xmax = 0.5, ymax = 13.5, ymin = 7.5, fill = colorAG[1])+
  geom_rect(xmin = -11, xmax = 0.5, ymax = 7.5, ymin = 6.5, fill = colorDemo[1])+
  geom_rect(xmin = -11, xmax = 0.5, ymax = 6.5, ymin = 0.5, fill = colorACALOS[1])+
  geom_vline(xintercept = c(20,32), color = "green", linetype = "dashed")+
  # bottom labels
  geom_labelpath(data = labelACALOS, aes(x,y, label = z), size = 2.5, fill =colorACALOS[1], color = colorACALOS[2], fontface = 2 )+
  geom_labelpath(data = labelAG, aes(x,y, label = z), size = 2.5, fill =colorAG[1], color = colorAG[2], fontface = 2 )+
  geom_labelpath(data = labelAnam, aes(x,y, label = z), size = 2.5, fill =colorDemo[1], color = colorDemo[2], fontface = 2 )+
  geom_labelpath(data = labelSpeech, aes(x,y, label = z), size = 2.5, fill =colorSpeech[1], color = colorSpeech[2], fontface = 2 , size = 5)+
  # brackets outside for table indication
  geom_brace(aes(y = c(15.6,16), x=c(min(merging_areas$A), max(merging_areas$A)), label="A"), inherit.data=F, labelsize=3)+
  geom_brace(aes(y = c(15.6,16), x=c(min(merging_areas$B), max(merging_areas$B)), label="B"), inherit.data=F, labelsize=3)+
  geom_brace(aes(y = c(16.3,17), x=c(min(merging_areas$C), max(merging_areas$C)), label="C"), inherit.data=F, labelsize=3)+
  geom_brace(aes(y = c(15.6,16), x=c(min(merging_areas$D), max(merging_areas$D)), label="D"), inherit.data=F, labelsize=3)+
  geom_brace(aes(y = c(17.5,18), x=c(min(merging_areas$E), max(merging_areas$E)), label="E"), inherit.data=F, labelsize=3)+
  coord_cartesian(xlim = range(0.5,info$merges + 0.5), ylim=c(0.5,15.5), clip = "off") +
  theme_bw() +
  theme(text = element_text(size = text_size),
        legend.position = c(0.45,-0.2),
        legend.direction = "horizontal",
        legend.text = element_text(size = text_size),
        legend.title = element_text(size = text_size),
        strip.placement = "none",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = c(colorSpeech[1], colorAG[1], colorDemo[1], colorACALOS[1]) ),  # Make facet label background white.
        panel.spacing = unit(0, units = "cm"),
        plot.margin = unit(c(0.1, 0.01, 0.1, 0.05), units="npc")) +
  scale_fill_gradient2(name = "Overlap", breaks = c(0.25,0.5,0.75), label = c("0.25", "0.5", "0.75"),  low = "darkblue", mid = "lightblue", midpoint = 0.5, high = "darkred")+
  scale_x_continuous(name = "Iteration", expand = c(0.0,0.0))+
  scale_y_discrete(expand = c(0,0))+
  ylab("")+
  ggtitle("")

# PLOT 1 & PLOT 2 --------------------------------------------------------------
p_cutoffIter_heatmap = ggarrange(p_cutoff_iteration+ theme(plot.margin = unit(c(0.15, 0.06, 0.0, 0.01), units="npc")) ,
          p_mergeFeaturesHeatmap + theme( plot.margin = unit(c(0.1, 0.033, 0.11, 0.01), units="npc")),
          ncol = 2, 
          widths = c(0.7,1),
          labels = c("A", "B"))

ggsave(filename = "plots/2_profile_merging/merging_iterations.png", plot =p_cutoffIter_heatmap, width = 19, height = 13, units = "cm")


# PLOT 3 FEATURE IMPORTANCE ACROSS MERGING AREAS--------------------------------

# make the merging areas groups
group_df = as.data.frame(t(data.frame(
  A = round(colMeans(all_feat[merging_areas$A,c(feat_names,"mean_overlap")]),3),
  B = round(colMeans(all_feat[merging_areas$B,c(feat_names,"mean_overlap")]),3),
  C = round(colMeans(all_feat[merging_areas$C,c(feat_names,"mean_overlap")]),3),
  D = round(colMeans(all_feat[merging_areas$D,c(feat_names,"mean_overlap")] ),3),
  E = round(colMeans(all_feat[merging_areas$E,c(feat_names,"mean_overlap")]),3)
)))

# 1 - value to make higher values more important & then divide by mean overlap 
group_df = 1-group_df
group_df = group_df[,feat_names]/group_df$mean_overlap


group_stack = stack(group_df[,feat_names])
group_stack$set = rep(c("A", "B", "C", "D","E"), times = length(feat_names))
names(group_stack)= c("values", "features", "set")

group_stack$features = factor(group_stack$features, 
                              levels = rev(feat_name_order))
group_stack$imp = ifelse(group_stack$values >=1, "Yes", "No")


# define the curved labels 
xpos = -2.7

# transformation to start at 1
t_shift <- scales::trans_new("shift",
                             transform = function(x) {x-1},
                             inverse = function(x) {x+1})

p_mergingAreaFeatureImp = ggplot(data = group_stack, aes(x = values, y = features) )+
  # color rectangulars
  geom_rect(data = group_stack[group_stack$set %in% "A",],xmin = xpos, xmax = -1.1, ymax = 14.5, ymin =13.5,fill = c(colorSpeech[1]))+
  geom_rect(data = group_stack[group_stack$set %in% "A",],xmin = xpos, xmax = -1.1, ymax = 13.5, ymin = 7.5, fill = colorAG[1])+
  geom_rect(data = group_stack[group_stack$set %in% "A",],xmin = xpos, xmax = -1.1, ymax = 7.5, ymin = 6.5, fill = colorDemo[1])+
  geom_rect(data = group_stack[group_stack$set %in% "A",],xmin = xpos, xmax = -1.1, ymax = 6.5, ymin = 0.5, fill = colorACALOS[1])+
  geom_bar(stat = "identity", aes(fill = imp)) +
  geom_vline(xintercept = 1, color = "black")+
  theme_bw() +
  coord_cartesian(xlim = range(0,2), ylim= c(1,14), clip = "off") +
  theme(text = element_text(size = text_size),
        plot.margin = unit(c(0.01, 0.01, 0, 0.04), units="npc"), 
        legend.position = "none") +
  # plot labels
  geom_labelpath(data = group_stack[group_stack$set %in% "A",], x=xpos,y =3.5, label = "Loudness scaling",size = 2.5, fill =colorACALOS[1], color = colorACALOS[2], fontface = 2, angle = 90 )+
  geom_labelpath(data = group_stack[group_stack$set %in% "A",], x=xpos,y = 10.5, label = "Audiogram", size = 2.5, fill =colorAG[1], color = colorAG[2], fontface = 2 , angle = 90)+
  geom_labelpath(data = group_stack[group_stack$set %in% "A",], x=xpos,y = 7, label = "Anamnesis", size = 2.5, fill =colorDemo[1], color = colorDemo[2], fontface = 2, angle = 90 )+
  geom_labelpath(data = group_stack[group_stack$set %in% "A",], x=xpos,y = 14, label = "Speech test", size = 2.5, fill =colorSpeech[1], color = colorSpeech[2], fontface = 2, angle = 90)+
  scale_x_continuous(trans = t_shift)+
  scale_y_discrete(drop = FALSE)+
  xlab("Importance score")+
  ylab("")+
  facet_grid(~set)

ggsave(p_mergingAreaFeatureImp, filename= "plots/2_profile_merging/mergingAreaFeatureImp.png", units = "cm", width = 17, height = 10)


# SELECT PROFILE SET -----------------------------------------------------------

pset = 32 # select based on plots and stats
save(pset, file="data/2_profile_merging/pset.Rdata")




