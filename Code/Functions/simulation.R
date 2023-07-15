# ---------------------------------------------------- #
# the following is to run simulation with replications #
# ---------------------------------------------------- #

simulation <- function(N,
                       phi_v,
                       pattern,
                       res_probability_prior,
                       res_probability_all,
                       prob_pattern,
                       differsite = 0,
                       R) {
  
  ##############################################################
  ## Generate simulated trial data and priors for Bayesian analysis
  ##############################################################
  
  no_pattern <<- length(pattern)
  # number of patterns
  
  no_comparison <<-
    sapply(1:no_pattern, function(i) {
      length(pattern[[i]]) - 1
    })
  # for each pattern, the number of pairwise comparisons fixing a reference treatment
  
  no_treatment <<- length(unique(unlist(pattern)))
  # number of unique treatments in the overall trial
  
  #res_probability_all<-matrix(rep(response_prob_V, no_pattern), ncol = no_treatment, byrow = T)
  colnames(res_probability_all) <-
    sapply(1:no_treatment, function(i) {
      paste0("treatment_", i)
    })
  rownames(res_probability_all) <-
    sapply(1:no_pattern, function(i) {
      paste0("alpha_", i)
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
  lambda <- prob_pattern # true prevalence rate of patterns
  
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
  
  
  output_replication <- lapply(1:R, function(k) {
    print(paste0('running iteration..', k))
    
    sim_data = gen.data(
      no_pattern,
      size_pattern,
      pattern,
      res_probability_prior,
      res_probability_all,
      differsite
    )
    
    ##############################################################
    ## Frequentist and Bayesian analysis
    ##############################################################
    
    # For Bayesian Framework, we must choose weight we wish to assign to prior distribution.
    # We explore both a weakly & strongly informative prior distribution
    Scale_wk <- 5     #Larger scale assigns less importance to prior distribution
    Scale_str <- 1    #Smaller scale assigns more importance to prior distribution
    
    # Fixed effects models
    est_method_C <- fit_onestage_C(sim_data[['trial_data']]) # use current trial data
    est_method_C_NI <- fit_onestage_C_NI(sim_data[['trial_data']]) # use current trial data, Bayesian
    est_method_C_wk <- fit_onestage_C_prior(sim_data[['prior_data']], sim_data[['trial_data']], Scale_wk) # use current trial data + prior data, Bayesian
    est_method_C_str <- fit_onestage_C_prior(sim_data[['prior_data']], sim_data[['trial_data']], Scale_str) # use current trial data + prior data, Bayesian
    
    # Use a hierarchical structure
    est_method_C2 <-
      fit_onestage_C_hier(sim_data[['trial_data']]) # use current trial data
    est_method_C2_NI <-
      fit_onestage_C_hier_NI(sim_data[['trial_data']]) # use current trial data, Bayesian
    est_method_C2_wk <-
      fit_onestage_C_hier_prior(sim_data[['prior_data']], sim_data[['trial_data']], Scale_wk) # use current trial data + prior data, Bayesian
    est_method_C2_str <-
      fit_onestage_C_hier_prior(sim_data[['prior_data']], sim_data[['trial_data']], Scale_str) # use current trial data + prior data, Bayesian
    
    ##############################################################
    ## Ranking of treatments
    ##############################################################
    
    # combine estimated best treatments from all methods, row = methods, column = pattern
    identified_best_t <- rbind(
      method_C = est_method_C$ranking[1, ],
      method_C_NI = est_method_C_NI$ranking[1, ],
      method_C_wk = est_method_C_wk$ranking[1, ],
      method_C_str = est_method_C_str$ranking[1, ],
      method_C2 = est_method_C2$ranking[1, ],
      method_C2_NI = est_method_C2_NI$ranking[1, ],
      method_C2_wk = est_method_C2_wk$ranking[1, ],
      method_C2_str = est_method_C2_str$ranking[1, ]
    )
    
    n_method <- dim(identified_best_t)[1] # how many methods compared 
    
    # true treatment effect for each pattern(column) from each method (row)
    identify_best_rate <- sapply(1:no_pattern, identify_bestR)
    
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
    
    best_treatment_I <- diff_min == 0
    
    nearbest_treatment_5 <- diff_min - 0.05 <= 0
    nearbest_treatment_10 <- diff_min - 0.1 <= 0
    
    rownames(mortality_gain) <-
      rownames(mortality_gain_ratio) <-
      rownames(better_treatment_I) <- rownames(identified_best_t)
    rownames(diff_min) <-
      rownames(best_treatment_I) <- rownames(identified_best_t)
    
    rownames(nearbest_treatment_5) <-
      rownames(nearbest_treatment_10) <- rownames(identified_best_t)
    
    estimand2 <- list(
      mortality_gain = mortality_gain,
      mortality_gain_ratio = mortality_gain_ratio,
      better_treatment_I = better_treatment_I,
      best_treatment_I = best_treatment_I,
      nearbest_treatment_5 = nearbest_treatment_5,
      nearbest_treatment_10 = nearbest_treatment_10,
      diff_min = diff_min
    )
    #names(measure)<-c("pattern1", "pattern2", "pattern3", "pattern4", "pattern5", "pattern6", "pattern7", "pattern8")
    
    # identify which models did not fit 
    identify_fail <- rbind(
      method_C = est_method_C$ranking[2, ],
      method_C_NI = est_method_C_NI$ranking[2, ],
      method_C_wk = est_method_C_wk$ranking[2, ],
      method_C_str = est_method_C_str$ranking[2, ],
      method_C2 = est_method_C2$ranking[2, ],
      method_C2_NI = est_method_C2_NI$ranking[2, ],
      method_C2_wk = est_method_C2_wk$ranking[2, ],
      method_C2_str = est_method_C2_str$ranking[2, ]
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
      est_method_C = est_method_C$contrast.est,
      est_method_C_NI = est_method_C_NI$contrast.est,
      est_method_C_wk = est_method_C_wk$contrast.est,
      est_method_C_str = est_method_C_str$contrast.est,
      est_method_C2 = est_method_C2$contrast.est,
      est_method_C2_NI = est_method_C2_NI$contrast.est,
      est_method_C2_wk = est_method_C2_wk$contrast.est,
      est_method_C2_str = est_method_C2_str$contrast.est,
      performance_m = estimand2,
      identify_fail = identify_fail,
      freq_t_subgroup = sim_data[['freq_t_subgroup']],
      freq_t = sim_data[['freq_t']]
    )
  })
  
  return(output_replication)
}