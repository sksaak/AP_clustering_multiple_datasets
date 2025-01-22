plot_profile_polar = function(df, features, featureGroup, profileColumn, profileIDX, changeFeatureDirection = NULL, ref){
  # Plots a polar plot for a single profile
  #
  # df = data.frame
  # features = features that should be plotted, order of features should be according to desired feature order, 
  #            as these will be the factor levels for ordering
  # featureGROUP = vector of which measurement each feature belongs to. Needed for labeling in the plot
  # profileColumn = column name in df that contains the profile labels
  # profileIDX = name of profile that should be plotted
  # changeFeatureDirection = feature names of features where the directionality needs to be changed. Higher scores generally
  #                           are worse, for acalos_diff_1000 & 4000 it is the other way around - therefore change! 
  # ref = Normal hearing refence scores. Need to be medians and scaled
  #
  #
  # Note: for dataset comparability minMaxScaling needs to include predefined minMax scores that remain the same across datasets 
  
  # color palette
  cols = c("darkgreen","yellow","red")
  mypalette <- colorRampPalette(cols)(255)
  # plotting parameters
  colorSpeech = c("#DEEAF6","#004FA1")
  colorAG = c("#E2F0D9","#223E04")
  colorACALOS = c("#FFF2CD","#AC6E01")
  colorDemo = c("#F2F2F2","#5C565A")
  
  color_measurements_dark = c(rep(colorAG[2], times = 4),colorDemo[2], rep(colorACALOS[2], times =6), colorSpeech[2])
  color_measurements_light = c(rep(colorAG[1], times = 4),colorDemo[1], rep(colorACALOS[1], times =6), colorSpeech[1])
  
  # get true medians for stats 
  profile = df[df[,profileColumn] == profileIDX, features ]
  orig_median = apply(profile,2,median)
  
  # normalize with minMax for comparability  
  scaled_df = as.data.frame(minMaxScale(df[,features]))
  scaled_profile =  scaled_df[df[,profileColumn] == profileIDX, features ]   
  
  
  # change direction of certain variables, if indicated
  if (!is.null(changeFeatureDirection)){
    for (f in changeFeatureDirection){
      scaled_profile[,f] = 1- scaled_profile[,f]
    }
  }
  
  
  # get median & standard deviation for each feature
  stats_df = data.frame(name = factor(names(scaled_profile), levels = features),
                        median = apply(scaled_profile,2,median),
                        sd = apply(scaled_profile,2,sd))
  
  # reference median to the normal hearing reference group 
  stats_df$median = stats_df$median - ref
  stats_df$group = featureGroup
  
  # actual plot 
  
  p = ggplot(data = stats_df, aes(x = name, y = median, fill = median))+
    geom_bar(stat="identity")+
    geom_hline(yintercept = 0, color = "darkgreen")+
    geom_textpath(aes(y = 0.8,label=orig_median), color = color_measurements_dark)+
    theme_bw() +
    theme(text = element_text(size = text_size),
          title = element_text(face = "bold"),
          plot.margin = unit(c(0,0,0,0), "cm"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.ontop =FALSE,
          axis.text.x = element_text(vjust =0, face = "bold",colour = color_measurements_dark),
          axis.title.x = element_blank(),
          legend.key.size = unit(1, "cm"),
          legend.text = element_text(size =  text_size),
          legend.title = element_text(size = text_size, face = "bold"))+
    scale_fill_gradientn(colours = mypalette,limits = c(-0.3,0.85), breaks = c(-0.2,0,0.2,0.4,0.6,0.8), guide = guide_colourbar(direction = "horizontal"))+
    # scale_x_discrete(labels = c("AC PTA", "ABG", "ASYM", "UCL PTA","Age","1_L15", "1_L35", "1_diff", "4_L15", "4_L35","4_diff","S0N0_bin"))+
    ylim(-0.25,1)+
    ylab("")+ xlab("")+
    labs(title = paste("Profile:", profileIDX),
         caption = paste0("N = ", nrow(scaled_profile) ))+
    coord_curvedpolar(clip = "off") +
    geom_labelpath(data = labelAG, aes(x,y, label = z), size = 2.5, fill =colorAG[1], color = colorAG[2], fontface = 2 )+
    geom_labelpath(data = labelACALOS, aes(x,y, label = z), size = 2.5, fill =colorACALOS[1], color = colorACALOS[2], fontface = 2 )+
    geom_labelpath(data = labelAnam, aes(x,y, label = z), size = 2.5, fill =colorDemo[1], color = colorDemo[2], fontface = 2 )+
    geom_labelpath(data = labelSpeech, aes(x,y, label = z), size = 2.5, fill =colorSpeech[1], color = colorSpeech[2], fontface = 2 )
  
  
  return(p)
}

