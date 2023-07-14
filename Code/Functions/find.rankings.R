# -------------------------------------------------------------- #
# identify suggested treatment for a pattern given the contrasts #
# -------------------------------------------------------------- #

find.rankings<-function(t_labelv, treat.coeff){ 
  #treat.coeff has size=no_treatment
  #t_labelv=t_label; treat.coeff=est.contrasts
  
  if(all(is.na(treat.coeff))){ # if all est.contrasts are NA
    out<-sample(t_labelv,1) # random recommendation
    not_fit<-1 # record model not fitted
  }else{ 
    out <- which.min(treat.coeff)
    not_fit<-0
  }
  
  # if model fitted then give 
  # out = treatment with the lowest coefficient 
  # not fit = 0 means model has fitted
  return(c(out, not_fit))
}
