plot_boxplot = function(data, y, x, fill, xlims, xlabel,title){
  # plotting function for basic profile plot
  p = ggplot(data = data, aes(y = data[,y], x = data[,x], fill = data[,fill])) +
    geom_boxplot() +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(legend.position = "none") +
    xlim(xlims[1], xlims[2])+
    xlab(xlabel)+
    ylab("Profile")+
    ggtitle(title)
  
  return(p)
}