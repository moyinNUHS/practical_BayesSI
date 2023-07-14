# for each subgroup, prepare the coefficients to identify rankings

prep.coeff <- function(i) {
  # extract trial data from each pattern
  sub_data <- alldata[, i]
  
  # treatments in the pattern
  t_label <- sort(unique(sub_data$treatment_label))
  
  if (is.null(my.glm$error))
    # if there is no error in model fit (there could be warning)
  {
    # extract coefficients for each treatment
    fit.coeff <- c(0, out[, 1])
    #fit.coeff <- append(out[,1], 0, after=(Treat.best-1))
    est.contrasts <- rep(NA, no_treatment)
    est.contrasts[t_label] <- fit.coeff[t_label]
    
  } else {
    est.contrasts <- rep(NA, no_treatment)
    
  }
  
  return(find.rankings(t_labelv = t_label,
                       treat.coeff = est.contrasts))
  
}