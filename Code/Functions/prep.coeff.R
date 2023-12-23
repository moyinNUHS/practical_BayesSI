# for each subgroup, prepare the coefficients to identify rankings

prep.coeff <- function(i, Trial_Treat_lab_vec, model, out) {
  # extract all treatment labels from each pattern
  sub_data <- Trial_Treat_lab_vec[[i]]
  
  # summarise treatments in the pattern
  t_label <- sort(unique(sub_data))
  
  if (is.null(model$error))
    # if there is no error in model fit (there could be warning)
  {
    # extract coefficients for each treatment
    fit.coeff <- out[, 1]
    est.contrasts <- rep(NA, no_treatment)
    est.contrasts[t_label] <- fit.coeff[t_label]
    
  } else {
    #Empty vector if error in model fit
    est.contrasts <- rep(NA, no_treatment)
    
  }
  
  #Use function 'find.rankings' to return recommended "best" or random treatment if model is fit or not
  return(find.rankings(t_labelv = t_label,
                       treat.coeff = est.contrasts))
  
}
