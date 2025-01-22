merge_lists = function(lists, identifier = NULL){
  # Merge profile lists 
  # identifier, should an identifier be added that shows the list number?
  # identifier  = NULL (no identifier), "lsidx" (numeric identifier available), "lsname" (character identfier available)
  #               
  df = c()
  for (l in 1:length(lists)){
    
    out = lists[[l]]
    
    if (identifier=="lsidx" ){out$mergeProfile = l}
    if (identifier=="lsname"){out$mergeProfile = names(lists[l])}
    
    df = rbind(df, out)
    
  }
  return(df)
}