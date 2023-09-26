# ---------------------------------------------------- #
# the following is to run simulation with replications #
# ---------------------------------------------------- #

simulation <- function(N,      
                       N_hist,
                       phi_v,
                       pattern,
                       res_probability_prior,
                       res_probability_prior_ur1,
                       res_probability_prior_ur2,
                       res_probability_all,
                       res_probability_prior_site,
                       res_probability_prior_ur1_site,
                       res_probability_prior_ur2_site,
                       res_probability_all_site,
                       prob_pattern,
                       differsite = 0,
                       R) {
  
  ##############################################################
  ## Generate simulated trial data and priors for Bayesian analysis
  ##############################################################
  
  no_pattern <<- length(pattern)
  # number of patterns
  
  no_treatment <<- length(unique(unlist(pattern)))
  # number of unique treatments in the overall trial
  
  #res_probability_all<-matrix(rep(response_prob_V, no_pattern), ncol = no_treatment, byrow = T)
  colnames(res_probability_all) <-
    sapply(1:no_treatment, function(i) {
      paste0("treatment_", i)
    })
  # response rate: row = pattern, column = treatment
  
  # generate which pattern each patient in N patients belong to
  # each person has prob_pattern to be allocated to one of the treatment patterns
  assigned_pattern <- t(rmultinom(N, size = 1, prob_pattern))
  colnames(assigned_pattern) <-
    sapply(1:no_pattern, function(i) {
      paste0("subgroup", i)
    })
  
  # number of patients in each subgroup that is defined by the pattern
  size_pattern <<- apply(assigned_pattern, 2, sum)

  # generate which pattern each historical patient in N_hist patients belong to
  # each historical person has prob_pattern to be allocated to one of the treatment patterns
  assigned_pattern_hist <- t(rmultinom(N_hist, size = 1, prob_pattern))
  colnames(assigned_pattern_hist) <-
    sapply(1:no_pattern, function(i) {
      paste0("subgroup", i)
    })
  
  # number of patients in each subgroup that is defined by the pattern
  size_pattern_hist <<- apply(assigned_pattern_hist, 2, sum)
  
  true.response.r <-
    lapply(1:no_pattern, function(i)
      res_probability_all[i, pattern[[i]]])
  # generates a list of treatments effects per treatment in each pattern
  
  true.mean.min <- lapply(1:no_pattern, function(i) {
    v <- true.response.r[[i]]
    c("mean" = mean(v), "min" = min(v))
  })
  # computes the mean and minimum values of the treatment effects in each pattern
  true.mean.min <- do.call(cbind, true.mean.min)
  # matrix of mean and min values of the treatment effects in each pattern
  # will be used for the performance measures about the treatment decisions
  
  # run simulation and analysis over R iterations
  output_replication <- lapply(1:R, function(k) {
    
    print(paste0('running iteration..', k))
    
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
    # We explore both a weakly & strongly informative prior distribution
    Scale_wk <- 5     #Larger scale assigns less importance to prior distribution
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
    
    #Extract treatment labels
    Trial_Treat_lab_vec<-apply(sim_data[['trial_data']], 2, function(x) x$treatment_label)
    
    #Extract frequencies
    freq_t_subgroup_list<-sim_data[['freq_t_subgroup']]
    freq_t_list<-sim_data[['freq_t']]
    
    #Delete sim_data
    rm(sim_data)
    
    # Fixed effects models
    est_method_1 <- fit_model_1(nma_data, Trial_Treat_lab_vec) # use current trial data
    est_method_1_NI <- fit_model_1_NI(nma_data, Trial_Treat_lab_vec) # use current trial data, Bayesian
    est_method_1_wk <- fit_model_1_prior(nma_data_prior, nma_data, Trial_Treat_lab_vec, Scale = Scale_wk) # use current trial data + prior data, Bayesian
    est_method_1_str <- fit_model_1_prior(nma_data_prior, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data, Bayesian
    est_method_1_wk_ur1 <- fit_model_1_prior(nma_data_prior_ur1, nma_data, Trial_Treat_lab_vec, Scale = Scale_wk) # use current trial data + prior data ur1, Bayesian
    est_method_1_str_ur1 <- fit_model_1_prior(nma_data_prior_ur1, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data ur1, Bayesian
    est_method_1_wk_ur2 <- fit_model_1_prior(nma_data_prior_ur2, nma_data, Trial_Treat_lab_vec, Scale = Scale_wk) # use current trial data + prior data ur2, Bayesian
    est_method_1_str_ur2 <- fit_model_1_prior(nma_data_prior_ur2, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data ur2, Bayesian
    
    # Use a hierarchical structure
    est_method_2 <-fit_model_2(nma_data, Trial_Treat_lab_vec) # use current trial data
    est_method_2_NI <-fit_model_2_NI(nma_data, Trial_Treat_lab_vec) # use current trial data, Bayesian
    est_method_2_wk <-fit_model_2_prior(nma_data_prior, nma_data, Trial_Treat_lab_vec, Scale = Scale_wk) # use current trial data + prior data, Bayesian
    est_method_2_str <-fit_model_2_prior(nma_data_prior, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data, Bayesian
    est_method_2_wk_ur1 <- fit_model_2_prior(nma_data_prior_ur1, nma_data, Trial_Treat_lab_vec, Scale = Scale_wk) # use current trial data + prior data ur1, Bayesian
    est_method_2_str_ur1 <- fit_model_2_prior(nma_data_prior_ur1, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data ur1, Bayesian
    est_method_2_wk_ur2 <- fit_model_2_prior(nma_data_prior_ur2, nma_data, Trial_Treat_lab_vec, Scale = Scale_wk) # use current trial data + prior data ur2, Bayesian
    est_method_2_str_ur2 <- fit_model_2_prior(nma_data_prior_ur2, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data ur2, Bayesian
    
    ##############################################################
    ## Ranking of treatments
    ##############################################################
    
    # combine estimated best treatments from all methods, row = methods, column = pattern
    identified_best_t <- rbind(
      method_1 = est_method_1$ranking[1, ],
      method_1_NI = est_method_1_NI$ranking[1, ],
      method_1_wk = est_method_1_wk$ranking[1, ],
      method_1_str = est_method_1_str$ranking[1, ],
      method_1_wk_ur1 = est_method_1_wk_ur1$ranking[1, ],
      method_1_str_ur1 = est_method_1_str_ur1$ranking[1, ],
      method_1_wk_ur2 = est_method_1_wk_ur2$ranking[1, ],
      method_1_str_ur2 = est_method_1_str_ur2$ranking[1, ],
      method_2 = est_method_2$ranking[1, ],
      method_2_NI = est_method_2_NI$ranking[1, ],
      method_2_wk = est_method_2_wk$ranking[1, ],
      method_2_str = est_method_2_str$ranking[1, ],
      method_2_wk_ur1 = est_method_2_wk_ur1$ranking[1, ],
      method_2_str_ur1 = est_method_2_str_ur1$ranking[1, ],
      method_2_wk_ur2 = est_method_2_wk_ur2$ranking[1, ],
      method_2_str_ur2 = est_method_2_str_ur2$ranking[1, ]
    )
    
    n_method <- dim(identified_best_t)[1] # how many methods compared 
    
    # true treatment effect for each pattern(column) from each method (row)
    identify_best_rate <- sapply(1:no_pattern, identify_bestR, identified_best_t, res_probability_all)
    
    # compute mortality reduction for each pattern(column) from each method (row)
    mortality_gain <- t(sapply(1:n_method, function(m) {
      identify_best_rate[m, ] - true.mean.min[1, ]
    }))
    better_treatment_I <- mortality_gain < 0
    
    # compute maximum possible mortality reduction for each pattern(column) from each method (row) if true best treatment known
    mortality_gain_max <- true.mean.min[2, ] - true.mean.min[1, ]
    
    # compute maximum possible mortality reduction ratio of each method vs if true best known
    mortality_gain_ratio <-
      t(sapply(1:n_method, function(m) {
        mortality_gain[m, ] / mortality_gain_max
      }))
    
    diff_min <-
      t(sapply(1:n_method, function(m) {
        identify_best_rate[m, ] - true.mean.min[2, ]
      }))
    
    #   best_treatment_I <- diff_min == 0
    
    nearbest_treatment_5 <- diff_min - 0.05 <= 0
    #  nearbest_treatment_10 <- diff_min - 0.1 <= 0
    
    rownames(mortality_gain) <-
      rownames(mortality_gain_ratio) <-
      rownames(better_treatment_I) <- 
      rownames(diff_min) <- 
      #    rownames(best_treatment_I) <- 
      #  rownames(nearbest_treatment_10) <- 
      rownames(nearbest_treatment_5) <- rownames(identified_best_t)
    
    estimand2 <- list(
      mortality_gain = mortality_gain,
      mortality_gain_ratio = mortality_gain_ratio,
      better_treatment_I = better_treatment_I,
      #   best_treatment_I = best_treatment_I,
      nearbest_treatment_5 = nearbest_treatment_5,
      #   nearbest_treatment_10 = nearbest_treatment_10,
      diff_min = diff_min
    )
    
    # identify which models did not fit 
    identify_fail <- rbind(
      method_1 = est_method_1$ranking[2, ],
      method_1_NI = est_method_1_NI$ranking[2, ],
      method_1_wk = est_method_1_wk$ranking[2, ],
      method_1_str = est_method_1_str$ranking[2, ],
      method_1_wk_ur1 = est_method_1_wk_ur1$ranking[2, ],
      method_1_str_ur1 = est_method_1_str_ur1$ranking[2, ],
      method_1_wk_ur2 = est_method_1_wk_ur2$ranking[2, ],
      method_1_str_ur2 = est_method_1_str_ur2$ranking[2, ],
      method_2 = est_method_2$ranking[2, ],
      method_2_NI = est_method_2_NI$ranking[2, ],
      method_2_wk = est_method_2_wk$ranking[2, ],
      method_2_str = est_method_2_str$ranking[2, ],
      method_2_wk_ur1 = est_method_2_wk_ur1$ranking[2, ],
      method_2_str_ur1 = est_method_2_str_ur1$ranking[2, ],
      method_2_wk_ur2 = est_method_2_wk_ur2$ranking[2, ],
      method_2_str_ur2 = est_method_2_str_ur2$ranking[2, ]
    )
    
    # print errors if a model did not fit 
    # if (any(identify_fail > 0)) {
    #   row.fail = which(identify_fail == 1, arr.ind = TRUE)[,1]
    #   col.fail = which(identify_fail == 1, arr.ind = TRUE)[,2]
    #   message('The following models did not fit: ', 
    #           paste('|', rownames(identify_fail)[row.fail],
    #                 colnames(identify_fail)[col.fail], '|'))
    # } else {
    #   message('All models fitted..')
    # }
    
    # output of all results 
    list(
      identified_best_t = identified_best_t,
      est_method_1 = est_method_1$contrast.est,
      est_method_1_NI = est_method_1_NI$contrast.est,
      est_method_1_wk = est_method_1_wk$contrast.est,
      est_method_1_str = est_method_1_str$contrast.est,
      est_method_1_wk_ur1 = est_method_1_wk_ur1$contrast.est,
      est_method_1_str_ur1 = est_method_1_str_ur1$contrast.est,
      est_method_1_wk_ur2 = est_method_1_wk_ur2$contrast.est,
      est_method_1_str_ur2 = est_method_1_str_ur2$contrast.est,
      
      est_method_2 = est_method_2$contrast.est,
      est_method_2_NI = est_method_2_NI$contrast.est,
      est_method_2_wk = est_method_2_wk$contrast.est,
      est_method_2_str = est_method_2_str$contrast.est,
      est_method_2_wk_ur1 = est_method_2_wk_ur1$contrast.est,
      est_method_2_str_ur1 = est_method_2_str_ur1$contrast.est,
      est_method_2_wk_ur2 = est_method_2_wk_ur2$contrast.est,
      est_method_2_str_ur2 = est_method_2_str_ur2$contrast.est,
      performance_m = estimand2,
      identify_fail = identify_fail,
      freq_t_subgroup = freq_t_subgroup_list
      freq_t = freq_t_list
    )
  })
  
  return(output_replication)
}
