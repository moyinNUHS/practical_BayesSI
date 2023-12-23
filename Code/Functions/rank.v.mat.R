rank.v.mat <- function(no_p, Trial_Treat_lab_vec, my.glm, out){
  # gives a matrix where 
  # 1st row = best treatments 
  # 2nd row indicates 1 if any models did not fit 
  rank.v <- sapply(1:no_p, function(x) {prep.coeff(x, Trial_Treat_lab_vec, my.glm, out)})

  #Label columns and rows
  colnames(rank.v) <- sapply(1:no_pattern, function(i) paste("pattern", i))
  row.names(rank.v) <- c("suggested treatment", "model.not.fit")
  
  return(rank.v)
}
