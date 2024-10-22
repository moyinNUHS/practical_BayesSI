# ---------------------------------------------------- #
# the following is to run simulation with replications #
# ---------------------------------------------------- #
#Specify sample size(s) for each run
sample_size = c(500, 1000, 1500, 3000, 5000)

#Specify sample size of historical clinical trial
N_hist=500

#Specify number of iterations for each scenario
No_iter = 1000

simulation <- function(N,      
                       N_hist,
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
                       R,
                       scenario_name) {
  
  ##############################################################
  ## Generate simulated trial data and priors for Bayesian analysis
  ##############################################################
  
  no_pattern <- length(pattern)
  # number of patterns
  
  no_treatment <- length(unique(unlist(pattern)))
  # number of unique treatments in the overall trial
  
  colnames(res_probability_all) <-
    sapply(1:no_treatment, function(i) {
      paste0("treatment_", i)
    })
  # response rate: row = pattern, column = treatment
  
  # generate which pattern each patient in N patients belong to
  # each person has prob_pattern to be allocated to one of the treatment patterns
  
  set.seed(1) ###check with HPC 
  
  assigned_pattern <- t(rmultinom(N, size = 1, prob_pattern))
  colnames(assigned_pattern) <-
    sapply(1:no_pattern, function(i) {
      paste0("subgroup", i)
    })
  
  # number of patients in each subgroup that is defined by the pattern
  size_pattern <- apply(assigned_pattern, 2, sum)
  
  # generate which pattern each historical patient in N_hist patients belong to
  # each historical person has prob_pattern to be allocated to one of the treatment patterns
  assigned_pattern_hist <- t(rmultinom(N_hist, size = 1, prob_pattern))
  colnames(assigned_pattern_hist) <-
    sapply(1:no_pattern, function(i) {
      paste0("subgroup", i)
    })
  
  # number of patients in each subgroup that is defined by the pattern
  size_pattern_hist <- apply(assigned_pattern_hist, 2, sum)
  
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
  
  ###matrix to store warning messages per iteration:
  messages <- as.data.frame(matrix(NA, 1, 3))
  colnames(messages) <- c("V1", "k", "N")
  
  ##matrix to store error messages per iteration 
  errors <- as.data.frame(matrix(NA, 1, 3))
  colnames(errors) <- c("V1", "k", "N")
  
  .GlobalEnv$no_pattern <- no_pattern
  .GlobalEnv$no_treatment <- no_treatment
  .GlobalEnv$size_pattern <- size_pattern
  .GlobalEnv$size_pattern_hist <- size_pattern_hist
  
  # run simulation and analysis over R iterations
  output_replication <- foreach(k = 1:R) %dopar% {
    set.seed(k)
    print(paste0('running iteration..', k))
    
    library(tidymodels)
    library(parsnip)
    library(pacman)
    library(stringr)
    library(multiwayvcov)
    library(sandwich)
    library(purrr)
    library(insight)
    library(modelsummary)
    library(multilevelmod)
    library(lme4)
    library(multcomp)
    library(rstanarm)
    
    wd = '/Users/cheryl/Documents/Documents - Xinru’s MacBook Pro (2)/duke-nus/bibhas/practical/practical/'
    #wd = '/data/chakraborty/home/e0859927/practical/'
    setwd(wd)
    scripts = paste0(wd, 'Code/Functions/', list.files('Code/Functions/')[which(list.files('Code/Functions/')!="simulation.R")])
    lapply(scripts, source)
    
    .GlobalEnv$no_pattern <- no_pattern
    .GlobalEnv$no_treatment <- no_treatment
    .GlobalEnv$size_pattern <- size_pattern
    .GlobalEnv$size_pattern_hist <- size_pattern_hist
    
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
    

      # Fixed effects models
      est_method_1 <- fit_model_1(nma_data, Trial_Treat_lab_vec) # use current trial data
      #est_method_1_wk <- fit_model_1_weakly(nma_data, Trial_Treat_lab_vec) # use current trial data + prior data, Bayesian
      est_method_1_str <- fit_model_1_prior(nma_data_prior, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data, Bayesian
      est_method_1_str_ur1 <- fit_model_1_prior(nma_data_prior_ur1, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data ur1, Bayesian
      est_method_1_str_ur2 <- fit_model_1_prior(nma_data_prior_ur2, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data ur2, Bayesian
      
      # Use a hierarchical structure
      est_method_2 <-fit_model_2(nma_data, Trial_Treat_lab_vec) # use current trial data
      #est_method_2_wk <-fit_model_2_weakly(nma_data, Trial_Treat_lab_vec) # use current trial data + prior data, Bayesian
      est_method_2_str <-fit_model_2_prior(nma_data_prior, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data, Bayesian
      est_method_2_str_ur1 <- fit_model_2_prior(nma_data_prior_ur1, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data ur1, Bayesian
      est_method_2_str_ur2 <- fit_model_2_prior(nma_data_prior_ur2, nma_data, Trial_Treat_lab_vec, Scale = Scale_str) # use current trial data + prior data ur2, Bayesian
      
      ##############################################################
      ## Ranking of treatments
      ##############################################################
      
      # combine estimated best treatments from all methods, row = methods, column = pattern
      identified_best_t <- rbind(
        method_1 = est_method_1$ranking[1, ],
        #method_1_wk = est_method_1_wk$ranking[1, ],
        method_1_str = est_method_1_str$ranking[1, ],
        method_1_str_ur1 = est_method_1_str_ur1$ranking[1, ],
        method_1_str_ur2 = est_method_1_str_ur2$ranking[1, ],
        method_2 = est_method_2$ranking[1, ],
        #method_2_wk = est_method_2_wk$ranking[1, ],
        method_2_str = est_method_2_str$ranking[1, ],
        method_2_str_ur1 = est_method_2_str_ur1$ranking[1, ],
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
    
    
    nearbest_treatment_5 <- diff_min - 0.05 <= 0
    
    rownames(mortality_gain) <-
      rownames(mortality_gain_ratio) <-
      rownames(better_treatment_I) <- 
      rownames(diff_min) <- 
      
      rownames(nearbest_treatment_5) <- rownames(identified_best_t)
    
    estimand2 <- list(
      mortality_gain = mortality_gain,
      mortality_gain_ratio = mortality_gain_ratio,
      better_treatment_I = better_treatment_I,
      nearbest_treatment_5 = nearbest_treatment_5,
      diff_min = diff_min
    )
    
   
      # identify which models did not fit 
      identify_fail <- rbind(
        method_1 = est_method_1$ranking[2, ],
        #method_1_wk = est_method_1_wk$ranking[2, ],
        method_1_str = est_method_1_str$ranking[2, ],
        method_1_str_ur1 = est_method_1_str_ur1$ranking[2, ],
        method_1_str_ur2 = est_method_1_str_ur2$ranking[2, ],
        method_2 = est_method_2$ranking[2, ],
        #method_2_wk = est_method_2_wk$ranking[2, ],
        method_2_str = est_method_2_str$ranking[2, ],
        method_2_str_ur1 = est_method_2_str_ur1$ranking[2, ],
        method_2_str_ur2 = est_method_2_str_ur2$ranking[2, ]
      )
      
      messages_per_iter <- rbind(
        method_1 = est_method_1$warn,
        #method_1_wk = est_method_1_wk$warn,
        method_1_str = est_method_1_str$warn,
        method_1_str_ur1= est_method_1_str_ur1$warn,
        method_1_str_ur2 = est_method_1_str_ur2$warn,
        method_2 = est_method_2$warn,
        #method_2_wk = est_method_2_wk$warn,
        method_2_str = est_method_2_str$warn,
        method_2_str_ur1= est_method_2_str_ur1$warn,
        method_2_str_ur2 = est_method_2_str_ur2$warn
      )
      
      if(!is_empty(messages_per_iter)){
        messages_per_iter2 <- as.data.frame(cbind(messages_per_iter, k, N))
      }
      
      if (is_empty(messages_per_iter)){
        messages_per_iter2 <- as.data.frame(matrix(NA, 1, ncol(messages)))
        colnames(messages_per_iter2) <- colnames(messages)
      }
      
      messages <- rbind(messages, messages_per_iter2)
      
      errors_per_iter <- rbind(
        method_1 = est_method_1$error,
        #method_1_wk = est_method_1_wk$error,
        method_1_str = est_method_1_str$error,
        method_1_str_ur1= est_method_1_str_ur1$error,
        method_1_str_ur2 = est_method_1_str_ur2$error,
        method_2 = est_method_2$error,
        #method_2_wk = est_method_2_wk$error,
        method_2_str = est_method_2_str$error,
        method_2_str_ur1= est_method_2_str_ur1$error,
        method_2_str_ur2 = est_method_2_str_ur2$error
      )
      
      if(!is_empty(errors_per_iter)){
        errors_per_iter2 <- as.data.frame(cbind(errors_per_iter, k, N))
      }
      
      if (is_empty(errors_per_iter)){
        errors_per_iter2 <- as.data.frame(matrix(NA, 1, ncol(errors)))
        colnames(errors_per_iter2) <- colnames(errors)
      }
      
      errors <- rbind(errors, errors_per_iter2)
      
      # output of all results 
      list(
        identified_best_t = identified_best_t,
        est_method_1 = est_method_1$contrast.est,
        #est_method_1_wk = est_method_1_wk$contrast.est,
        est_method_1_str = est_method_1_str$contrast.est,
        est_method_1_str_ur1 = est_method_1_str_ur1$contrast.est,
        est_method_1_str_ur2 = est_method_1_str_ur2$contrast.est,
        
        est_method_2 = est_method_2$contrast.est,
        #est_method_2_wk = est_method_2_wk$contrast.est,
        est_method_2_str = est_method_2_str$contrast.est,
        est_method_2_str_ur1 = est_method_2_str_ur1$contrast.est,
        est_method_2_str_ur2 = est_method_2_str_ur2$contrast.est,
        
        performance_m = estimand2,
        identify_fail = identify_fail,
        warn = messages,
        errors = errors,
        freq_t_subgroup = freq_t_subgroup_list,
        freq_t = freq_t_list
      )
    }
    
  
  return(output_replication)
}
