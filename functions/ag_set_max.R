# set audiograms to maximum if measures stop and previous freq was above 100
# at Hörzentrum NA was inserted if frequency could not be measures due to severity of hearing loss
# becomes visible if previous frequencies are available and already high
ag_set_max <- function(x,min=80,max = 120){
  x <- as.numeric(x)

  # get idx of last present value
  val = tail(x[!is.na(x)], n = 1)

  # if indication, impute, else ignore
  if (sum(is.na(x)) != length(x) & sum(is.na(x)) >= 1){
    if (val > min){

      x[is.na(x)] = max

    }
  } else{
    x = x
  }

  return(x)
}
