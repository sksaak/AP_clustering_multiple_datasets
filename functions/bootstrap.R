
bootstrap <- function(df, method ="subsample", perc = 0.9){
  # bootstrap a dataframe, either with "resample" or "subsample"
  #
  # df = data.frame to be bootstrapped
  # method = c("resample", "subsample")
  # perc = only relevant for method "subsample", the size of the subsample
  #
  # returns bootstrapped data.frame
  
  df <- as.data.frame(df)
  
  if (method == "resample"){ 
    ids = sort(sample(1:nrow(df),size = nrow(df), replace = TRUE))
  }else if (method == "subsample"){
    ids = sort(sample(1:nrow(df),size = round(nrow(df)*perc), replace = FALSE))
  }
  
  new_dat <- c()
  for (r in ids){
    new_dat <- rbind(new_dat, df[r,])
  }
  
  return(new_dat)
}
