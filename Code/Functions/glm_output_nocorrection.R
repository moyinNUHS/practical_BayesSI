### generate glm output table without any type 1 error correction

glm_output_nocorrection <- function(model, t1_error=0.2) { # unadjusted type 1 error

  #summary of model
  mof <- summary(model)
  #Get standard errors
  std.err.naive <- mof$coefficients[1:no_treatment, "Std. Error"]
  
  q.val.naive <- qnorm(1 - t1_error/2)
  
  out <- cbind(Estimate = coefficients(mof)[1:no_treatment],
               model_var = std.err.naive^2,
               z = abs(coefficients(mof)[1:no_treatment] / std.err.naive),
               LL = coefficients(mof)[1:no_treatment] - q.val.naive  * std.err.naive,
               UL = coefficients(mof)[1:no_treatment] + q.val.naive  * std.err.naive)
  
  return(out)
}
