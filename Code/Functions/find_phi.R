# ------------------------------- #
# generate data for each subgroup #
# ------------------------------- #

find_phi<-function(p, alpha){
  log(p/(1-p)) - alpha 
} 
