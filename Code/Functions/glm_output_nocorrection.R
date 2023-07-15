### generate glm output table without any type 1 error correction

glm_output_nocorrection <- function(model, t1_error = 0.05, alt_hypothesis = 'two.sided') { # unadjusted type 1 error
  
  mof <- summary(model)
  std.err.naive <- mof$coefficients[2:no_treatment, "Std. Error"]

  if (alt_hypothesis != 'two.sided') {t1_error = 0.1}
  
  q.val.naive <- qnorm(1 - t1_error/2)
  
  out <- cbind(Estimate = coefficients(mof)[2:no_treatment],
               model_var = std.err.naive^2,
               z = abs(coefficients(mof)[2:no_treatment] / std.err.naive),
               LL = coefficients(mof)[2:no_treatment] - q.val.naive  * std.err.naive,
               UL = coefficients(mof)[2:no_treatment] + q.val.naive  * std.err.naive)
  
  return(out)
}