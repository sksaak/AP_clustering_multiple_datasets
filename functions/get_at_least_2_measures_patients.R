get_at_least_2_measures_patients = function(data){
  # checks that there is at least data from two measurements available 
  # for further imputation 
  # can be adjusted according to the available measures in a given dataset 
  
  
  ag =  data[,c("ASYM", "ABG", "UCL_PTA","AC_PTA", "bisgaard")]
  goesa = data %>% select(starts_with("goesa"))
  aca = data %>% select(starts_with("acalos"))
  
  
  missing_measure = data.frame(ag = rowSums(is.na(ag)) == ncol(ag),
                               goesa = rowSums(is.na(goesa)) == ncol(goesa),
                               aca = rowSums(is.na(aca)) == ncol(aca)
  )
  
  id_rm = rowSums(missing_measure) > 1 # needs to be adjusted if more measures are included
  
  
  data = data[!id_rm,]
  
  return(data)
  
}