# compute the property of estimator

com_property <- function(out_one, # matrix of simulation outputs (estimator, model variance, Z score, UL and LL) [rows] against iterations [cols]
                         q, # the treatment 
                         n_method, # number of methods
                         T_v, R, scenario_name) { # pre-defined coefficient for each treatment

  
  if (all(is.na(out_one[, 1]))) {
    
    rep(NA, n_method)
    
  } else {
    
    scenario_name <- str_extract(scenario_name, "scenario\\d+\\.\\d+") 
    
    if(!(scenario_name %in% c("scenario4.1", "scenario4.2", "scenario4.3"))){
      # estimators
      val <- out_one[, 'Estimate']
      val <- as.numeric(val[complete.cases(val)])
      
      t.target <- logit(T_v[q]) #transform the probabilities T_v to coef in logistic regression
      # T_v is the pre-defined mortality of treatment effect 
    
      # bias is the difference between the estimated treatment coefficients (val) and the predefined treatment coefficients (t.target)
      bias <- mean(val - t.target)
      
      # variance between the estimated ORs obtained from all iterations
      var.s <- var(val)
      
      # means of model variances obtained from all iterations
      meanv2 <- mean(as.numeric(out_one[, 'model_var']), na.rm = T)
      
      v2 <- out_one[, c('model_var', 'LL', 'UL')]
      #meanv2<-apply(v2, 2, function(x){
      #  if(all(is.numeric(x))){mean(x)}else{
      #    indx<-which(is.na(str_extract(x, "[0-9]+")))
      #    mean(as.numeric(x[-indx])) } }  )
      
      # identify upper bound estimate(s) which is/are not a number
      pw <- v2[which(is.na(str_extract(v2[, 'UL'], "[0-9]+"))), 'UL']
      
      # if upper bound estimates are more or equal to predefined treatment coefficients
      # if lower bound estimates are less or equal to predefined treatment coefficients
      coverage_ind <- rbind(out_one[, 'UL'] >= t.target, 
                            out_one[, 'LL'] <= t.target)
      coverage_count <- apply(coverage_ind, 2, sum)
      coverage_prob <- length(which(coverage_count == 2)) / length(which(is.na(coverage_count) == F))
      
      # Mean squared error
      MSE <- mean((val - t.target) ^ 2)
      # Monte Carlo Standard Error
      MCSE_mse <- sqrt(var((val - t.target) ^ 2) / R)
      
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
          ex_model_var = meanv2,
          fail.no = length(which(is.na(out_one[, 1])))
        )
      )
    }
    else{
      v2 <- out_one[, c('model_var', 'LL', 'UL')]
      #meanv2<-apply(v2, 2, function(x){
      #  if(all(is.numeric(x))){mean(x)}else{
      #    indx<-which(is.na(str_extract(x, "[0-9]+")))
      #    mean(as.numeric(x[-indx])) } }  )
      
      # identify upper bound estimate(s) which is/are not a number
      pw <- v2[which(is.na(str_extract(v2[, 'UL'], "[0-9]+"))), 'UL']
      
      list(
        pw,
        c(
          fail.no = length(which(is.na(out_one[, 1])))
        )
      )
    }
  }
}
