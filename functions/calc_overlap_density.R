calc_overlap_density <- function(x1,x2, plot = FALSE, limits){
  # calculates the overlapping density between two distributions
  # 
  # x1 = vec 1
  # x2 = vec 2
  # plot = defaults to false, set to true if plot it required
  # limits = variable limits
  
  
  x <- list(x1 = x1, x2 = x2)
  out <- overlapping::overlap(x,plot=plot, boundaries = limits)
  
  out <- out$OV
  return(out)
} 
