# compute the property of estimator

com_property <- function(out_one, # matrix of simulation outputs (estimator, model variance, Z score, UL and LL) [rows] against iterations [cols]
                         q, # the treatment 
                         n_method, # number of methods
                         T_v, # pre-defined coefficient for each treatment
                         R, # Total number of simulations
                         scenario_name, # Scenario label
                         m) { # Method for Model prediction

  value<-"method_2"
  if (all(is.na(out_one[, 1]))) {
    
    rep(NA, n_method) #Output a vector of 'NA's if all R simulations produce 'NA' for the prediction of treatment q's coefficient for method m
    
  } else {
    
    scenario_name <- str_extract(scenario_name, "scenario\\d+\\.\\d+") 
    
    if(!(scenario_name %in% c("scenario4.1", "scenario4.2", "scenario4.3"))){
      # estimators
      val <- out_one[, 'Estimate']
      val <- as.numeric(val[complete.cases(val)])
      val.prob<-invlogit(val)       #transform the coefficents to probabilities
      
      t.target <- logit(T_v[q]) #transform the probabilities T_v to coef in logistic regression
      # T_v is the pre-defined mortality of treatment effect 
    
      # bias is the difference between the estimated treatment coefficients (val) and the predefined treatment coefficients (t.target)
      bias <- mean(val.prob - T_v[q])
      
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
      MSE <- mean((val.prob - T_v[q]) ^ 2)
      # Monte Carlo Standard Error
      #MCSE_mse <- sqrt(var((val - t.target) ^ 2) / R)
      
      list(
        pw,
        c(
          bias = bias,
          empirical_var = var.s,
          coverage_prob = coverage_prob,
          mse = MSE,
          MCSE_bias = sqrt(var.s / R),
          #MCSE_cov_p = sqrt(coverage_prob * (1 - coverage_prob) / R),
          #MCSE_MSE = MCSE_mse,
          ex_model_var = meanv2,
          fail.no = length(which(is.na(out_one[, 1])))
        )
      )
    } 
    else if ( scenario_name %in% c("scenario4.1")) {
      # estimators
      val <- out_one[, 'Estimate']
      val <- as.numeric(val[complete.cases(val)])
      val.prob<-invlogit(val)       #transform the coefficents to probabilities

      if (grepl(value, m)) {
        T_v<-invlogit(c(-0.01628043, 0.2749548, 0.5248289, 0.7546663))  
        #For probabilities: 
        #S1: [0.199, 0.250, 0.299, 0.350]
        #S2: [0.291, 0.354, 0.413, 0.470]
        #S3: [0.648, 0.711, 0.760, 0.799]
        #S4: [0.833, 0.870, 0.896, 0.915]
      }

      t.target <- logit(T_v[q]) #transform the probabilities T_v to coef in logistic regression
      # T_v is the pre-defined mortality of treatment effect 
    
      # bias is the difference between the estimated treatment coefficients (val) and the predefined treatment coefficients (t.target)
      bias <- mean(val.prob - T_v[q])
      
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
      MSE <- mean((val.prob - T_v[q]) ^ 2)
      # Monte Carlo Standard Error
      #MCSE_mse <- sqrt(var((val - t.target) ^ 2) / R)
      
      list(
        pw,
        c(
          bias = bias,
          empirical_var = var.s,
          coverage_prob = coverage_prob,
          mse = MSE,
          MCSE_bias = sqrt(var.s / R),
          #MCSE_cov_p = sqrt(coverage_prob * (1 - coverage_prob) / R),
          #MCSE_MSE = MCSE_mse,
          ex_model_var = meanv2,
          fail.no = length(which(is.na(out_one[, 1])))
        )
      )
    } 
    else if ( scenario_name %in% c("scenario4.2")) {
# estimators
      val <- out_one[, 'Estimate']
      val <- as.numeric(val[complete.cases(val)])
      val.prob<-invlogit(val)       #transform the coefficents to probabilities

      T_v<-(0.2*(T_v-0.1))+(0.8*T_v)
      #T_v<-(0.4*(T_v-0.1))+(0.6*T_v) for scenario4.3
      
      t.target <- logit(T_v[q]) #transform the probabilities T_v to coef in logistic regression
      # T_v is the pre-defined mortality of treatment effect 
    
      # bias is the difference between the estimated treatment coefficients (val) and the predefined treatment coefficients (t.target)
      bias <- mean(val.prob - T_v[q])
      
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
      MSE <- mean((val.prob - T_v[q]) ^ 2)
      # Monte Carlo Standard Error
      #MCSE_mse <- sqrt(var((val - t.target) ^ 2) / R)
      
      list(
        pw,
        c(
          bias = bias,
          empirical_var = var.s,
          coverage_prob = coverage_prob,
          mse = MSE,
          MCSE_bias = sqrt(var.s / R),
          #MCSE_cov_p = sqrt(coverage_prob * (1 - coverage_prob) / R),
          #MCSE_MSE = MCSE_mse,
          ex_model_var = meanv2,
          fail.no = length(which(is.na(out_one[, 1])))
        )
      )
    } 
  #  else{
  #    v2 <- out_one[, c('model_var', 'LL', 'UL')]
  #    #meanv2<-apply(v2, 2, function(x){
  #    #  if(all(is.numeric(x))){mean(x)}else{
  #    #    indx<-which(is.na(str_extract(x, "[0-9]+")))
  #    #    mean(as.numeric(x[-indx])) } }  )
  #    
  #    # identify upper bound estimate(s) which is/are not a number
  #    pw <- v2[which(is.na(str_extract(v2[, 'UL'], "[0-9]+"))), 'UL']
  #    
  #    list(
  #      pw,
  #      c(
  #        fail.no = length(which(is.na(out_one[, 1])))
  #      )
  #    )
  #  }
  }
}
