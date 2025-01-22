order_profiles_by_test = function(profiles, test = "goesa_S0N0_bin"){
# Reorder profile labels, such that lower numbers indicate higher impairment and vice versa
#
# profiles = list of profiles
# test = audiological features upon which the ordering shall occur, defaults to goesa_S0N0_bin 

  # get profile order 
  profile_order = data.frame(test = unlist(lapply(profiles, function(x) { mean(x[,test], na.rm = TRUE)})),
                             original = c(1:length(profiles)))
  colnames(profile_order)[1] = test
  profile_order = profile_order[order(profile_order[,1]),]
  
  profile_order$new = length(profiles):1
  profile_order$rev = 1:length(profiles)
  
 
  # change profile names according to order
  tmp_profile_order = profile_order[order(profile_order[,2]),]
  
  # rename lists with new ordering 
  names(profiles) = tmp_profile_order$new
  
  # generate data.frame and make "mergeProfile" as factor 
  df = merge_lists(profiles, identifier = "lsname")
  df$mergeProfile = factor(df$mergeProfile, levels = 1:length(profiles))
  
  
  reorderedProfiles = list(profile_order = profile_order,
                           data = df, 
                           profiles = profiles)
  
  
  return(reorderedProfiles)

}








