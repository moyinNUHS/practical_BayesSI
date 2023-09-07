### generate glm output (Frequentist) with Bonferroni correction 

glm_output_bonferr <- function(model, p, no_treatment) {
  
  # Bonferroni correction
  mof <- summary(model)
  std.err.naive <- mof$coefficients[1:no_treatment, "Std. Error"]
  
  # get confidence intervals with adjusted p 
  K = length(coefficients(model))
  if (K == 1) { # the above line does not work for hierarchical model 
    K = length(coefficients(model)$subgroup) - 1 # minus 1 for intercept   
  }
  modGLHT = glht(model, linfct = diag(K))
  
  cf = confint(modGLHT, adjusted(type = "bonferroni"), level = 1 - p)$confint
  
  # output 
  out = cbind(Estimate = cf[1:no_treatment, 'Estimate'],
               model_var = std.err.naive^2,
               z = abs(coefficients(mof)[1:no_treatment] / std.err.naive),
               LL = cf[1:no_treatment, 'lwr'],
               UL = cf[1:no_treatment, 'upr'])
  rownames(out) = paste0('treatment', rownames(out))
  
  out[which(abs(out[,1])>12),] <- NA #parameter not converged is set to NA 
  
  return(out)
}