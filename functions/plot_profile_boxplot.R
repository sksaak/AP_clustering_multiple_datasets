plot_profile_boxplot = function(df,profile,feature,titlename, xlabel, yticklabels, ylabel, xlimits,text_size =11, title_size = 11, backgroundcolor = "white"){
  # plot profile information 
  #
  # df = data.frame
  # profile = profile information, used for y axis and fill
  # feature = audiological feature, used for x axis
  # titlename = name of the plot
  # xlabel = xlabel
  # xlimits = xlimits of the plot
  
  
  
  p = ggplot(df, aes(y = profile, x = feature, fill = profile))+
    geom_boxplot(position = position_dodge())+
    scale_fill_viridis_d() +
    theme_bw()+
    theme(legend.position = "none",
          text = element_text(size = text_size), 
          plot.title = element_text(size = title_size),
          plot.background = element_rect(fill = backgroundcolor))+
    scale_y_discrete(name = ylabel, labels = yticklabels)+
    xlab(xlabel)+
    xlim(xlimits)+
    coord_cartesian(xlim = xlimits,
                    clip = 'off')+  
    ylab(ylabel)+
    ggtitle(titlename)
  
  return(p)
  
  
}
