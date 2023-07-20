# identify the best treatment

identify_bestR <- function(k, identified_best_t, res_probability_all) {
  #v<-sapply(1:6, function(m){ which( pattern [[k]]==identified_best_t[m,k]) } )
  #v<-sapply(1:6, function(m){
  #  o1<-which( pattern [[k]]==identified_best_t[m,k])
  #  if(length(o1)==0){NA}else{o1}
  #} )
  
  #t.rate<-true.response.r[[k]]
  #t.rate<-t.rate[v]
  #names(t.rate)<-rownames(identified_best_t)
  
  v <- identified_best_t[, k]
  t.rate <- res_probability_all[k, v]
  return(t.rate)
}
