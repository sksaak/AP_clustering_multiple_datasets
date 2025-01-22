ag_interpolate <- function(x,min = 80, max, impute_upper = TRUE){
  x <- as.numeric(x)
  for (freq in 2:(length(x)-1)){# start with the second end with second last, as first and last are not imputed
    if (is.na(x[freq]) && !is.na(x[freq-1]) && !is.na(x[freq+1])){ # interpolation
      x[freq] <- pracma::linspace(x[freq-1], x[freq+1], 1)
    } else{ x = x}
  }

  if (impute_upper == TRUE){
    x = ag_set_max(x,min,max)
  }

  return(x)
}



