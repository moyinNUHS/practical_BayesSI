### generate glm output (Bayes) with no correction 

glm_output_stan_nocorrection <- function (model, no_treatment, p=0.2) {
  
  # get posterior intervals with unadjusted p 
  mof.naive <- posterior_interval(model, prob = 1 - p)
  
  # get standard error
  std.err.naive <- model$ses[1:no_treatment]
  
  out <- cbind(
    Estimate = model$coefficients[1:no_treatment],
    model_var = std.err.naive ^ 2,
    z = abs(model$coefficients[1:no_treatment] / std.err.naive),
    LL = mof.naive[1:no_treatment, 1],
    UL = mof.naive[1:no_treatment, 2]
  )
  
  return(out)
}
