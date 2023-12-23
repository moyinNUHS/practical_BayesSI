### generate glm output table using Dunnett stepdown procedure 

glm_output_dunnett <- function(model, alt_hypothesis = 'two.sided') {
  
  # Dunnett test 
  dunnett_test <- glht(model = model, 
                       linfct = mcp(treatment = "Dunnett"),
                       alternative = alt_hypothesis)
  
  stepdown <- summary(dunnett_test, test = adjusted(type = "free")) # Step-down Dunnett test
  stepdown.p <- stepdown$test$pvalues # extract the p-values 
  q.val <- qnorm(1 - stepdown.p/2) # z value
  q.val[which( q.val == Inf)] = 0.999 # for very small p values 
  
  # get standard error
  std.err <- stepdown$test$sigma # inflated std error to account for multiplicity
  
  out <- cbind(Estimate = stepdown$test$coefficients,
               model_var = std.err^2,
               z = q.val,
               LL = stepdown$test$coefficients - q.val  * std.err,
               UL = stepdown$test$coefficients + q.val  * std.err)
  
  out[which(abs(out[,1])>12),] <- NA # parameter not converged is set to NA 
  
  return(out)
}
