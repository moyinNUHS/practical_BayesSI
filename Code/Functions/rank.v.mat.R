# gives a matrix where 
# 1st row = best treatments 
# 2nd row indicates 1 if any models did not fit 

rank.v.mat <- function(no_p, alldata, my.glm, out){
  rank.v <- sapply(1:no_p, function(x) {prep.coeff(x, alldata, my.glm, out)})
  
  colnames(rank.v) <- sapply(1:no_pattern, function(i) paste("pattern", i))
  row.names(rank.v) <- c("suggested treatment", "model.not.fit")
  
  return(rank.v)
}