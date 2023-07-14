### generate glm output (Bayes) with Bonferroni correction 

glm_output_stan_bonferr <- function(model, p, no_treatment) {
  
  # Bonferroni correction
  adjusted.p <- p / no_treatment
  
  # get posterior intervals with adjusted p 
  mof <- posterior_interval(my.glmm, prob = 1 - adjusted.p)
  
  # get standard errors using posterior samples 
  posterior <- as.matrix(my.glmm)
  q.val <- qnorm(1 - adjusted.p / 2) # z score 
  std.err <- apply(posterior, 2, function(x) quantile(x, (1 - adjusted.p) / 2) / q.val)
  
  # output 
  out <- cbind(Estimate = my.glmm$coefficients[2:no_treatment],
               model_var = std.err[2:no_treatment]^2,
               z = q.val,
               LL = mof[2:no_treatment, 1],
               UL = mof[2:no_treatment, 2])
  
  out[which(abs(out[,1])>12),] <- NA #parameter not converged is set to NA 
  
  return(out)
}