normedCount = function(vec, from, to){
  # function that generates a vector of equidistant steps from "from" to "to"
  #
  # 
  
  breaks = seq(from = from, to = to,  by = (to-from)/100  )
  
  
  out =  hist(vec, breaks = breaks, plot = FALSE )
  
  return(out)
  
  
}