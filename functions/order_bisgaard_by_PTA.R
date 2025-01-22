order_bisgaard_by_PTA <- function(bisgaard){
  # reorder the bisgaard 
  
  bis_N = 10
  
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
  
  
  bisgaard_raw <- matrix(data = NA, nrow = 10, ncol = 1)
  
  # pta of bisgaard vectors
  for (i in 1:bis_N){
    
    bisgaard_raw[i] <- (bisgaard_profiles[2,i] + bisgaard_profiles[4,i] + bisgaard_profiles[6,i] + bisgaard_profiles[8,i]) / 4
  }
  
  bisgaard_raw <- as.data.frame(bisgaard_raw)
  bisgaard_raw$bis <-c(1:10)
  
  bisgaard_raw <- bisgaard_raw[order(bisgaard_raw$V1, decreasing = FALSE),]
  bisgaard_raw$order = c(101:110)
  
  #  bis_minMax <- as.data.frame(bisgaard_raw)
  
  bisgaard_scaled<- c()
  vec <- bisgaard_raw$bis
  
  for (row in 1:length(bisgaard)){
    
    for (bis in 1:bis_N){
      if(is.na(bisgaard[row])){
        bisgaard[row] = NA
        bisgaard_scaled[row] = NA
      } else if(bisgaard[row] == bisgaard_raw$bis[bis]){
        bisgaard_scaled[row] = bis
        next
      }
    }
  }
  
  
  return(bisgaard_scaled)
}