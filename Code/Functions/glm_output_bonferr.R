### generate glm output (Frequentist) with Bonferroni correction 

glm_output_bonferr <- function(model, p, no_treatment) {
  
  # Bonferroni correction
  
  mof <- summary(model)
  std.err.naive <- mof$coefficients[2:no_treatment, "Std. Error"]
  
  # get confidence intervals with adjusted p 
  K = length(coefficients(model))
  modGLHT = glht(model, linfct = diag(K))
  
  cf = confint(modGLHT, adjusted(type = "bonferroni"), level = 1 - p)$confint
  
  # output 
  out <- cbind(Estimate = model$coefficients[2:no_treatment],
               model_var = std.err.naive^2,
               z = abs(coefficients(mof)[2:no_treatment] / std.err.naive),
               LL = cf[2:no_treatment, 'lwr'],
               UL = cf[2:no_treatment, 'upr'])
  
  out[which(abs(out[,1])>12),] <- NA #parameter not converged is set to NA 
  
  return(out)
}