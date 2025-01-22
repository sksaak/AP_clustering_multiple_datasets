get_bisgaard <- function(data, bis_freqs){
  # calculates the bisgaard class for given audiogram shapes 
  # 
  # data = requires columns with audiogram data bisgaard frequencies
  # bis_freqs = column names for 250, 500, 750, 1000, 1500, 2000, 3000, 4000, 6000 Hz
  
  
  # bisgaard profiles 
  bisgaard_profiles <- data.frame(N1 = c(10, 10, 10,   10, 10, 15, 20, 30, 40), 
                                  N2 = c(20, 20, 22.5, 25, 30, 35, 40, 45, 50),
                                  N3 = c(35, 35, 35,   40, 45, 50, 55, 60, 65), 
                                  N4 = c(55, 55, 55,   55, 60, 65, 70, 75, 80), 
                                  N5 = c(65, 70, 72.5, 75, 80, 80, 80, 80, 80), 
                                  N6 = c(75, 80, 82.5, 85, 90, 90, 95, 100, 100),
                                  N7 = c(90, 95, 100, 105, 105, 105, 105, 105, 105),
                                  S1 = c(10, 10, 10,   10, 10, 15, 30, 55, 70),
                                  S2 = c(20, 20, 22.5, 25, 35, 55, 75, 95, 95),
                                  S3 = c(30, 35, 47.5, 60, 70, 75, 80, 80, 85) 
  )
  rownames(bisgaard_profiles) <- c(250, 500, 750, 1000, 1500, 2000, 3000, 4000, 6000)
  
  bisgaard <- NULL
  
  # loop over subjects of data
  for (s in 1:nrow(data)){
    
    eucl_ag <- NULL
    # get the profile with the smallest difference to patient audiogram
    for (b in 1:ncol(bisgaard_profiles)){

      if(sum(is.na(data[s,bis_freqs])) > 3)
      {eucl_ag[b] = NA}
      
      eucl_ag[b] <- stats::dist(rbind(data[s,bis_freqs], bisgaard_profiles[,b]), method = "euclidean")
      
    }
    
    if (sum(is.na(eucl_ag)) > 2){ idx = NA
    } else (idx <- which.min(eucl_ag))
    
    bisgaard[s] <- idx
  }
  
  return (bisgaard)
  
}