# -------------------------------------------------------------- #
# identify suggested treatment for a pattern given the contrasts #
# -------------------------------------------------------------- #

find.rankings <- function(t_labelv, treat.coeff) {
  #treat.coeff has size=no_treatment
  #t_labelv=t_label; treat.coeff=est.contrasts
  
  if (all(is.na(treat.coeff))) {
    # if all est.contrasts are NA
    out <- sample(t_labelv, 1) # random recommendation
    not_fit <- 1 # record model not fitted
  } else {
    #Find predicted "best" treatment (treatment with smallest coefficient)
    out<-which(treat.coeff == min(treat.coeff, na.rm = TRUE))
    if (length(out) > 1) {
      #if multiple treatments are "best" randomly pick between them
      out=sample(out, 1)
    }
    not_fit<-0 #record model is fitted
  }
  
  #Return either "best" or random treatment, if model is fitted or not
  return(c(out, not_fit))
}
