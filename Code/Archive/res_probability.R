# -------------------- #
# response probability #
# -------------------- #

res_probability<-function(phi,alpha){
  exp(alpha+ phi)/(1+ exp(alpha+ phi) )
}
