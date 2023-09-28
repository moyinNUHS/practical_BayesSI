# compute the property of estimator

com_property <- function(out_one, # matrix of simulation outputs (estimator, model variance, Z score, UL and LL) [rows] against iterations [cols]
                         q, # the treatment compared with the reference
                         n_method, # number of methods
                         phi_v, R) { # pre-defined OR of each treatment effect 
  
  if (all(is.na(out_one[, 1]))) {
    
    rep(NA, n_method)
    
  } else {
    
    # estimators
    val <- out_one[, 'Estimate']
    val <- as.numeric(val[complete.cases(val)])
    
    ################################################################
    #Yiyun to update here:
    #'val' the predicted coefficients from the model
    #'t.diff', (rename to t.target) 
    #So that predicted coefficents (val) and true 'target' coefficients (phi_v or t.target) have the same units (probability(?))
    #Due to fact we no longer have a reference treatment
    #################################################################
    
    # difference between the interested treatment (q) and reference treatment (1)
    # t.diff <- (phi_v[q] - phi_v[1])
    # phi_v is the pre-defined OR of each treatment effect 
    # phi_v[1] is the reference treatment effect in terms of OR
    t.diff <- logit(phi_v) #phi_v here is just T_vector, and we transform the probabilities to coef in logistic regression
    
    # bias is the difference between the estimated OR (val) and the predefined OR (t.diff)
    bias <- mean(val - t.diff)
    
    # variance between the estimated ORs obtained from all iterations
    var.s <- var(val)
    
    # means of model variances obtained from all iterations
    # meanv2 <- mean(as.numeric(out_one[, 'model_var']), na.rm = T)
    
    v2 <- out_one[, c('model_var', 'LL', 'UL')]
    #meanv2<-apply(v2, 2, function(x){
    #  if(all(is.numeric(x))){mean(x)}else{
    #    indx<-which(is.na(str_extract(x, "[0-9]+")))
    #    mean(as.numeric(x[-indx])) } }  )
    
    # identify upper bound estimate(s) which is/are not a number
    pw <- v2[which(is.na(str_extract(v2[, 'UL'], "[0-9]+"))), 'UL']
    
    # if upper bound estimates are more or equal to predefined OR
    # if lower bound estimates are less or equal to predefined OR
    coverage_ind <- rbind(out_one[, 'UL'] >= t.diff, 
                          out_one[, 'LL'] <= t.diff)
    coverage_count <- apply(coverage_ind, 2, sum)
    coverage_prob <- length(which(coverage_count == 2)) / length(which(is.na(coverage_count) == F))
    
    # Mean squared error
    MSE <- mean((val - t.diff) ^ 2)
    # Monte Carlo Standard Error
    MCSE_mse <- sqrt(var((val - t.diff) ^ 2) / R)
    
    list(
      pw,
      c(
        bias = bias,
        empirical_var = var.s,
        coverage_prob = coverage_prob,
        mse = MSE,
        MCSE_bias = sqrt(var.s / R),
        MCSE_cov_p = sqrt(coverage_prob * (1 - coverage_prob) / R),
        MCSE_MSE = MCSE_mse,
        # ex_model_var = meanv2,
        fail.no = length(which(is.na(out_one[, 1])))
      )
    )
  }
}
