### generate glm output (Bayes) with no correction 

glm_output_stan_nocorrection <- function (model, p, no_treatment) {
  
  # get posterior intervals with unadjusted p 
  mof.naive <- posterior_interval(model, prob = 1 - p)
  
  # get standard error
  std.err.naive <- model$ses[2:no_treatment]
  
  out <- cbind(
    Estimate = model$coefficients[2:no_treatment],
    #get_estimates(my.glmm, centrality = "mean")[2:no_treatment, 2], #for mean instead of median
    model_var = std.err.naive ^ 2,
    z = abs(my.glmm$coefficients[2:no_treatment] / std.err.naive),
    #get_estimates(my.glmm, centrality = "mean")[2:no_treatment, 2]/std.err,
    LL = mof.naive[2:no_treatment, 1],
    UL = mof.naive[2:no_treatment, 2]
  )
  out[which(abs(out[, 1]) > 12),] <- NA #parameter not converged is set to NA
  
  return(out)
}