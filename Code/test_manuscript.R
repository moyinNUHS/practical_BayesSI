fe_coef <-matrix(NA,nrow = 10,ncol = 7)
me_coef <-matrix(NA,nrow = 10,ncol = 4)
colnames(fe_coef) <- c("treatment1","treatment2","treatment3","treatment4","subgroup2","subgroup3","subgroup4")
colnames(me_coef) <- c("subgroup1","subgroup2","subgroup3","subgroup4")
for (i in 1:10) {
  set.seed(i)
  sim_data = gen.data(
    no_pattern,
    size_pattern,
    size_pattern_hist,
    pattern,
    res_probability_prior,
    res_probability_prior_ur1,
    res_probability_prior_ur2,
    res_probability_all,
    res_probability_prior_site,
    res_probability_prior_ur1_site,
    res_probability_prior_ur2_site,
    res_probability_all_site,
    differsite
  )
  
  ##############################################################
  ## Frequentist and Bayesian analysis
  ##############################################################
  
  # For Bayesian Framework, we must choose weight we wish to assign to prior distribution.
  # We explore both a very weakly (not based on HD) & strongly informative prior distribution
  Scale_str <- 1    #Smaller scale assigns more importance to prior distribution
  
  # generate prior historical data 
  # put historical data in a dataframe - outcome, treatment, pattern/subgroup
  nma_data_prior <- data.frame(
    y = unlist(sim_data[['prior_data']][1,]),
    treatment = factor(unlist(sim_data[['prior_data']][2,]), levels = sort(unique(
      unlist(sim_data[['prior_data']][2,])
    ))),
    subgroup = factor(unlist(sim_data[['prior_data']][3,])),
    site=factor(unlist(sim_data[['prior_data']][4,]))
  )
  
  # put historical data in a dataframe - outcome, treatment, pattern/subgroup
  nma_data_prior_ur1 <- data.frame(
    y = unlist(sim_data[['prior_data_ur1']][1,]),
    treatment = factor(unlist(sim_data[['prior_data_ur1']][2,]), levels = sort(unique(
      unlist(sim_data[['prior_data_ur1']][2,])
    ))),
    subgroup = factor(unlist(sim_data[['prior_data_ur1']][3,])),
    site=factor(unlist(sim_data[['prior_data']][4,]))
  )
  
  # put historical data in a dataframe - outcome, treatment, pattern/subgroup
  nma_data_prior_ur2 <- data.frame(
    y = unlist(sim_data[['prior_data_ur2']][1,]),
    treatment = factor(unlist(sim_data[['prior_data_ur2']][2,]), levels = sort(unique(
      unlist(sim_data[['prior_data_ur2']][2,])
    ))),
    subgroup = factor(unlist(sim_data[['prior_data_ur2']][3,])),
    site=factor(unlist(sim_data[['prior_data']][4,]))
  )
  
  # generate current data 
  # put current data in a dataframe - outcome, treatment, pattern/subgroup
  nma_data <- data.frame(
    y = unlist(sim_data[['trial_data']][1,]),
    treatment = factor(unlist(sim_data[['trial_data']][2,]), levels = sort(unique(
      unlist(sim_data[['trial_data']][2,])
    ))),
    subgroup = factor(unlist(sim_data[['trial_data']][3,])),
    site=factor(unlist(sim_data[['trial_data']][4,]))
  )
  
  
  ###################
  #Extract treatment labels
  Trial_Treat_lab_vec<-apply(sim_data[['trial_data']], 2, function(x) x$treatment_label)
  
  #Extract frequencies
  freq_t_subgroup_list<-sim_data[['freq_t_subgroup']]
  freq_t_list<-sim_data[['freq_t']]
  
  #Delete sim_data to save space 
  rm(sim_data)
  
  
  # number of patterns
  no_p <- no_pattern
  warn <- NULL 
  # model 
  my.glm <- myTryCatch(glm(y ~ -1 + treatment + subgroup, family = "binomial", data = nma_data) )
  
  fe_coef[i,] <- round(my.glm[[1]]$coefficients,digits = 10)
  
  # number of patterns
  no_p <- no_pattern
  warn <- NULL 
  
  # logistic regression
  my.glm <-
    myTryCatch(glmer(
      y ~ -1 + treatment + (1 | subgroup),
      family = "binomial",
      data = nma_data
    ))
  
  me_coef[i,] <- round(ranef(my.glm[[1]])$subgroup[,1],digits = 10)
  print(i)
}
apply(fe_coef, 2, mean)
apply(me_coef, 2, mean)
