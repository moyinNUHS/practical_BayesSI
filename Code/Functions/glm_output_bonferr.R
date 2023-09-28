### generate glm output (Frequentist) with Bonferroni correction 

glm_output_bonferr <- function(model, p, no_treatment) {
  
  mof <- summary(model)
  std.err.naive <- mof$coefficients[1:no_treatment, "Std. Error"]
  
  # Bonferroni correction
  adjusted.p = p / ncol(combn(no_treatment, 2))
  
  # get confidence intervals with adjusted p 
  cf = confint(model, level = 1 - adjusted.p)
  
  # output 
  out = cbind(Estimate = coef(mof)[1:no_treatment, 'Estimate'],
              model_var = std.err.naive^2,
              z = abs(coefficients(mof)[1:no_treatment] / std.err.naive),
              LL = cf[paste0('treatment', 1:no_treatment), 1],
              UL = cf[paste0('treatment', 1:no_treatment), 2])
  rownames(out) = paste0('treatment', rownames(out))
  
  out[which(abs(out[,1])>12),] <- NA #parameter not converged is set to NA 
  
  return(out)
}