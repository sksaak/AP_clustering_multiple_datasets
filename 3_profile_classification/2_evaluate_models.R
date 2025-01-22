#
#
# Evaluate models
#
#
###############################################################


# clear console, objects and plots 
cat("\014")  
rm(list = ls())

# libraries
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(ggpubr)) install.packages('ggpubr')
if(!require(caret)) install.packages('caret')
if(!require(RColorBrewer)) install.packages('RColorBrewer')
if(!require(geomtextpath)) install.packages('geomtextpath')
library(ggplot2)
library(ggpubr)
library(caret)
library(RColorBrewer)
library(geomtextpath)
library(patchwork)

# FUNCTIONS ------
source("functions/%not_in%.R")
source("functions/get_res_ova.R")
source("functions/get_res_dummy.R")
source("functions/predict_ova.R")
source("functions/calc_average_mets.R")

getmin = function(x){
  
  out = names(x[which.min(x)])
  out = as.numeric(out)
  out
  
}
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

set.seed(22)

# LOAD DATA --------------------------------------------------------------------
load("data/2_profile_merging/pset.Rdata")
load(paste0("data/2_profile_merging/profile_ordered_",pset,".Rdata"))

# PLOTTIMG PARAMETERS ----------------------------------------------------------
text_size = 9

# colors for categories 
colorSpeech = c("#DEEAF6","#004FA1")
colorAG = c("#E2F0D9","#223E04")
colorACALOS = c("#FFF2CD","#AC6E01")
colorDemo = c("#F2F2F2","#5C565A")

plotColors = c("#B2182B", "#B2182B","#B2182B", "orange", "#2166AC","#2166AC","orange","#2166AC","orange")
plotShapes = c(0,2,8, 0,0,2,2,8,8)

col_usecase = c("#993300", "#FF9900", "#FFCC33")
col_combined = c("#003300", "#00CC00", "#99CC00")
col_single = c("#000033", "#0000FF", "#3399CC")

col_blues = c("#00465e","#2171b5", "#6baed6")
col_greens = c("#0a642c", "#238b45", "#74c476")
col_red = c("#bb3e03", "#ca6702","#ee9b00")

col1 = "#B2182B"
col2 = "orange"
col3 = "#2166AC"

plotColors = c(col1,col2,col3, col1, col2,col3, col1, col2, col3)
plotColors = c(col_red, col_blues, col_greens)
plotColors = c(col_usecase, col_combined, col_single, "grey", "grey")

# GENERAL PARAMETERS -----------------------------------------------------------
m = "Kappa"
cv = c("repeatedCV")
feats = c("ALL", "APP", "HA", "AG","AG_GOESA","AG_ACALOS","GOESA", "GOESA_ACALOS", "ACALOS")
featureGroup = c("usecase","usecase","usecase","single","combined","combined", "single", "combined", "single") #c(1,1,1,2,3,3,2,3,2)
np = length(res$profiles)

all_ova_mets = c()
ova_pred_test = list()
ova_pred_train = list()
ova_pred_val = list()
i = 1

# LOOP ACROSS FEATURES ---------------------------------------------------------
for (feat in feats){

load(paste0("data/3_classification/train_parameters/",feat,"_train_params_",pset,".Rdata"))

# GET OVA RESULTS (TRAIN, VAL, TEST) -------------------------------------------
ova_mets_model = get_res_ova(feat = feat, p = pset, cond = "train", np = np)
ova_mets_train =  predict_ova(train_data, feat = feat, p = pset, np = np, m = m, cv = cv)
ova_mets_val   =  predict_ova(val_data, feat = feat, p = pset, np = np, m = m, cv = cv)
ova_mets_test  = predict_ova(test_data, feat = feat, p = pset, np = np, m = m, cv = cv)

ova_pred_train[[feat]] = data.frame(pred = ova_mets_train$pred, true = train_data$profile)
ova_pred_val[[feat]] = data.frame(pred = ova_mets_val$pred, true = val_data$profile)
ova_pred_test[[feat]] = data.frame(pred = ova_mets_test$pred, true = test_data$profile)

ova_mets_train= as.data.frame(ova_mets_train$metric)
ova_mets_val= as.data.frame(ova_mets_val$metric)
ova_mets_test= as.data.frame(ova_mets_test$metric)

# GET DUMMY MODEL RESULTS ------------------------------------------------------
dummy_mets = get_res_dummy(feat, p = pset, m = m)
names(dummy_mets) = c("Sensitivity","Specificity","Precision", "F1", "cond", "ap")

dummy_mets_train = stack(dummy_mets[dummy_mets$cond == "train", c(1:4)])
dummy_mets_train$ap = rep(1:np, times = 4)
dummy_mets_train$data = "train"
dummy_mets_train$cond = "dummy"

dummy_mets_val = stack(dummy_mets[dummy_mets$cond == "val", c(1:4)])
dummy_mets_val$ap = rep(1:np, times = 4)
dummy_mets_val$data = "val"
dummy_mets_val$cond = "dummy"

dummy_mets_test = stack(dummy_mets[dummy_mets$cond == "test", c(1:4)])
dummy_mets_test$ap = rep(1:np, times = 4)
dummy_mets_test$data = "test"
dummy_mets_test$cond = "dummy"

dummy_mets = rbind(dummy_mets_train, dummy_mets_val, dummy_mets_test)


# COMBINE OVA METS WITH DUMMY METS FOR PLOTTING --------------------------------
ova_mets_train= stack(ova_mets_train)
ova_mets_train$ap = rep(1:np, times = 4)
ova_mets_train$data = "train"

ova_mets_val= stack(ova_mets_val)
ova_mets_val$ap = rep(1:np, times = 4)
ova_mets_val$data = "val"

ova_mets_test= stack(ova_mets_test)
ova_mets_test$ap = rep(1:np, times = 4)
ova_mets_test$data = "test"

ova_mets = rbind(ova_mets_train, ova_mets_val, ova_mets_test )
ova_mets$cond = feat
ova_mets$color = plotColors[i]
ova_mets$shape = plotShapes[i]
ova_mets$group = featureGroup[i]

dummy_mets$color = "grey"
dummy_mets$shape = 1
dummy_mets$group = featureGroup[i]

ova_mets = rbind(ova_mets, dummy_mets)

names(ova_mets) = c("values", "metric", "ap", "data", "cond", "color", "shape","group")

all_ova_mets = rbind(all_ova_mets, ova_mets)

rm(dummy_mets, dummy_mets_test, dummy_mets_train, dummy_mets_val, ova_mets, ova_mets_train, ova_mets_test, ova_mets_val, ova_mets_model, fitControls)
i = i+1
}

# make factor of data source and remove specificity & F1
all_ova_mets$data = factor(all_ova_mets$data, levels = c("train", "val", "test"))
all_ova_mets = all_ova_mets[all_ova_mets$metric != "Specificity" & all_ova_mets$metric != "F1",]
all_ova_mets$metric = factor(all_ova_mets$metric, levels= c("Sensitivity", "Precision"))
all_ova_mets$ap = factor(all_ova_mets$ap, levels = res$profile_order$new)
all_ova_mets$cond = factor(all_ova_mets$cond, levels =c("ALL", "APP", "HA", "AG_GOESA","AG_ACALOS","GOESA_ACALOS", "AG","GOESA","ACALOS", "dummy") )
all_ova_mets$group = factor(all_ova_mets$group, levels =c("usecase", "combined","single") )

legendTitle = c("Usecase Combined Single")


# TRAIN PERFORMANCE ------------------------------------------------------------

dummy_train = all_ova_mets[all_ova_mets$cond == "dummy" & all_ova_mets$data == "train",]

p_all_mets_train= ggplot()+
  geom_line(data = dummy_train, aes(x = ap, y = values), group = 1, color = "grey")+
  geom_point(data = all_ova_mets[all_ova_mets$data == "train",], 
             aes(x = ap, y = values, color = cond, shape = cond),size = 2)+
  theme_bw()+
  scale_color_manual(values = plotColors)+
  scale_shape_manual(values = c(1:8,11,20))+
  theme(legend.position = "bottom",
        legend.title = element_text(size = 6, face = 2),
        text = element_text(size = text_size-2))+
  guides(color=guide_legend(nrow=3, byrow=FALSE,title.position = "top")) +
  ylab("Score")+
  xlab("AP")+
  ggtitle("Train performance")+
  facet_grid(metric ~ group)

# TEST PERFORMANCE--------------------------------------------------------------

dummy_test = all_ova_mets[all_ova_mets$cond == "dummy" & all_ova_mets$data == "test",]

p_all_mets_test= ggplot()+
  geom_line(data = dummy_test, aes(x = ap, y = values), group = 1, color = "grey")+
  geom_point(data = all_ova_mets[all_ova_mets$data == "test",], 
             aes(x = ap, y = values, color = cond, shape = cond),size = 2)+
  theme_bw()+
  scale_color_manual(values = plotColors)+
  scale_shape_manual(values = c(1:8,11,20))+
  theme(legend.position = "bottom",
        text = element_text(size = text_size))+
  guides(color=guide_legend(nrow=1, byrow=FALSE)) +
  ylab("Score")+
  xlab("AP")+
  ggtitle("Test performance")+
  facet_grid(metric ~ group)

p_legend = get_legend(p_all_mets_train)


# PLOT MEAN TEST PERFORMANCE ---------------------------------------------------
avTest =calc_average_mets(all_ova_mets, "test", c(feats,"dummy"))
avTest$group = factor(rep(c("usecase","usecase","usecase",
                            "combined", "combined", "combined",
                            "single", "single","single",
                            "dummy"), times = 2), levels = c("usecase","combined","single", "dummy"))
avTest$cond = factor(avTest$cond, levels= c("ALL","APP","HA","AG_GOESA","AG_ACALOS","GOESA_ACALOS","AG","GOESA","ACALOS", "dummy"))


p_meanTest =  ggplot(data = avTest, aes(x = ind, y = values, color = cond, group = cond, shape = cond))+
  geom_point(size = 2)+
  geom_line(linewidth = 0.2)+
  theme_bw()+
  theme(text = element_text(size = text_size))+
  scale_color_manual(values = plotColors) +
  scale_shape_manual(values = c(1:8,11,20))+
  theme(legend.position = "bottom")+
  guides(color=guide_legend(nrow=3, byrow=FALSE)) +
  xlab("Metric")+
  ylab("Score")+
  ylim(0,1)+
  ggtitle("Mean test performance") 
  
# COMBINED PLOT ----------------------------------------------------------------
p_performance_test = ggarrange(
  ggarrange(
    p_meanTest + theme(legend.position = "none"),
    p_legend,
    ncol = 2,
    widths = c(2, 1)
  ),
  
  p_all_mets_test  + theme(legend.position = "none"),
  ncol = 1,
  nrow = 2,
  heights = c(2, 3),
  labels = c("A", "B")
) + bgcolor("White")


p_performance_trainTest = ggarrange(
  # ROW 1
  ggarrange(
           p_all_mets_train + theme(legend.position = "none"), 
           p_all_mets_test + theme(legend.position = "none"),
           ncol = 2, labels = c("A", "B")),
  # Row 2
  ggarrange(
          p_meanTest + theme(legend.position = "none"),
          p_legend, labels = c("C", ""),
          ncol = 2),
  ncol = 1, nrow = 2, 
  heights = c(2,1))

ggsave(p_oldPerformance , filename = paste0("./plots/classification/trainTest_performance.png"),units = "in", width = 13, height = 8)



#- FEATURE IMPORTANCE CALCULATIONS----------------------------------------------

# get sample size from traindata 
samplesize = as.numeric(table(train_data$profile))

# adjust feat to desired feature set and name the columns accordingly
featImp_app =  get_featImp_ova(feat = "APP", p = pset, np = np)
names(featImp_app) = c("S0N0_bin", "AGE","AC PTA", "ASYM","BISGAARD","4_L15","4_L35","4_diff","1_L15", "1_L35", "1_diff","ap", "cond" )

# restructure featImp_app
names_feats_app = names(featImp_app[1:11])
feats_app = stack(featImp_app[,names_feats_app])
feats_app$ap = as.factor(rep(1:np, times = length(names_feats_app) ))
feats_app$cond = "APP"

feats_app$ap = factor(feats_app$ap, levels = res$profile_order$new)
feats_app$ind = factor(feats_app$ind, 
                       levels = rev(c("S0N0_bin","AC PTA", "ASYM","BISGAARD", "AGE",
                                      "1_L15", "1_L35","1_diff", "4_L15","4_L35","4_diff" ) )   )


# mean gini decrease transformed to 0/1 range for each profile to avoid sample size dependence in feature analysis
feats_app_norm = feats_app
feats_app_norm$ind = factor(feats_app_norm$ind, levels = c("S0N0_bin","AC PTA", "ASYM","BISGAARD", "AGE",
                                                               "1_L15", "1_L35","1_diff", "4_L15","4_L35","4_diff" ) )

for (p in 1:np){
  
  v = sum(feats_app_norm[feats_app_norm$ap == p,"values" ]) /100
  feats_app_norm$values[feats_app_norm$ap == p] = feats_app_norm$values[feats_app_norm$ap == p]/v
  
}

# add histogram on top to get estimate of averall feature importance, again all normalized to range from 0-100
feats_app_norm_all = feats_app_norm 

v = sum(feats_app_norm_all$values) /100
feats_app_norm_all$values = feats_app_norm_all$values/v

fs = c()
for (f in 1:length(levels(feats_app_norm_all$ind))){
  
  fsum = sum(feats_app_norm_all$values[feats_app_norm_all$ind == levels(feats_app_norm_all$ind)[f]])
  fs = c(fs,fsum)
}

mean_f = mean(fs)

# FEATURE IMPORTANCE PLOT ------------------------------------------------------

# mean gini decrease transformed to 0/1 range for each profile to avoid sample size dependence in feature analysis
p_vhcfeatsProfiles_norm = ggplot(data = feats_app_norm, aes(x = ind, y = values, fill = ap))+
  geom_bar(stat = "identity")+
  theme_bw()+
  # color rect 
  geom_rect(xmin = 0.4, xmax = 1.5, ymax = -2, ymin = -50,fill = c(colorSpeech[1]))+
  geom_rect(xmin = 1.5, xmax = 4.5, ymax = -2, ymin = -50, fill = colorAG[1])+
  geom_rect(xmin = 4.5, xmax = 5.5, ymax = -2, ymin = -50, fill = colorDemo[1])+
  geom_rect(xmin = 5.5, xmax = 11.6, ymax = -2, ymin = -50, fill = colorACALOS[1])+
  
  # # plot labels
  geom_labelpath(x=1,y = -50, label = "Speech test", size = 2.5, fill =colorSpeech[1], color = colorSpeech[2], fontface = 2, angle = 0)+
  geom_labelpath(x=3,y = -50, label = "Audiogram", size = 2.5, fill =colorAG[1], color = colorAG[2], fontface = 2 , angle = 0)+
  geom_labelpath(x=5,y = -50, label = "Anamnesis", size = 2.5, fill =colorDemo[1], color = colorDemo[2], fontface = 2, angle = 0)+
  geom_labelpath(x=8.5,y =-50, label = "Loudness scaling",size = 2.5, fill =colorACALOS[1], color = colorACALOS[2], fontface = 2, angle = 0 )+
  coord_cartesian(xlim = range(1,11), ylim= c(0,75), clip = "off") +
  theme(legend.position = "none",
        panel.spacing = unit(0,'lines'),
        text = element_text(size = text_size),
        plot.margin = unit(c(0.01, 0.01, 0.1, 0.01), units="npc"))+
  scale_fill_viridis_d(direction = 1)+
  scale_y_continuous(breaks = c(25,50,75))+
  ylab("Importance [%]")+
  xlab("")+
  facet_grid(feats_app$ap)

p_vhc_conds_norm_hist = ggplot(data = feats_app_norm_all, aes(x = ind, y = values))+
  geom_bar(stat = "identity", fill = "darkgrey", color = "darkgrey")+
  geom_hline(yintercept = mean_f, color = "black", linetype = "dashed")+
  theme_minimal()+
  theme(text = element_text(size = text_size),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(), 
        panel.grid = element_blank())+
  ylab("Overall importance [%]")
  
# combined plot 
p_featureIMPs = p_vhc_conds_norm_hist / p_vhcfeatsProfiles_norm + plot_layout(heights = c(0.15,0.85))


# SAVE PLOTS -------------------------------------------------------------------

# Classification performance
ggsave(p_performance_test, filename = paste0("plots/3_classification/test_performance.png"),units = "cm", width = 17, height = 17)
ggsave(p_performance_trainTest, filename = paste0("plots/3_classification/traintest_performance.png"),units = "cm", width = 17, height = 17)

# Feature importance 
ggsave(p_featureIMPs, filename= paste0("plots/3_classification/featureImpHIST.png"), units = "cm", width = 17, height = 20)
