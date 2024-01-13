## function to run one scenario

run_simulation <- function(pattern_list = list( # Treatment patterns
  pattern1 = c(2, 3), 
  pattern2 = c(1, 2, 3),
  pattern3 = c(2, 3, 4),
  pattern4 = c(1, 2, 3, 4)),
  prob_pattern,
  # prevalence of each pattern
  T_vector,
  # Treatment effects - first one being baseline
  res_rate_prior,
  # Priors
  res_rate_prior_ur1,
  # Priors
  res_rate_prior_ur2,
  # Priors
  samplesize_vec, 
  #Sample size for each simulation
  samplesize_hist,
  #Sample size for historical trial
  N_iter,
  # Number of iterations
  pattsame = TRUE,
  # if effects are the same across patterns,
  differsite = 0,
  #  how many sites have different effects,
  scenario_name) {
  
  store = list()
  for (N in samplesize_vec) {
    message(paste0('Starting simulation for sample size = ', N))
    
    if (pattsame) { # in the scenarios which there are no pattern effects 
      # make a matrix of pre-defined treatment effect per pattern for current trial
      res_rate_mat = matrix(
        T_vector,
        byrow = TRUE,
        nrow = length(pattern_list),
        # number of patterns
        ncol = length(T_vector)
      )     # number of treatments
      
      # make a matrix of prior treatment effect per pattern for representative historical trial
      res_rate_mat_prior = matrix(
        res_rate_prior,
        byrow = TRUE,
        nrow = length(pattern_list),
        ncol = length(res_rate_prior)
      )
      
      # make a matrix of prior treatment effect per pattern for unrepresentative historical trial 1
      res_rate_mat_prior_ur1 = matrix(
        res_rate_prior_ur1,
        byrow = TRUE,
        nrow = length(pattern_list),
        ncol = length(res_rate_prior_ur1)
      )
      
      # make a matrix of prior treatment effect per pattern for unrepresentative historical trial 2
      res_rate_mat_prior_ur2 = matrix(
        res_rate_prior_ur2,
        byrow = TRUE,
        nrow = length(pattern_list),
        ncol = length(res_rate_prior_ur2)
      )
    } 
    else {
     scenario_name. <- str_extract(scenario_name, "scenario\\d+\\.\\d+") 
     if ( scenario_name. %in% c("scenario4.1")) {
      # for scenario where treatments have equal effects but differ across patterns for current trial
      res_rate_mat = rbind(
        c(0.3, 0.3, 0.3, 0.3),
        # pattern 1
        c(0.35, 0.35, 0.35, 0.35),
        # pattern 2
        c(0.40, 0.40, 0.40, 0.40),
        # pattern 3
        c(0.45, 0.45, 0.45, 0.45)     
        # pattern 4
      )
      
      # for scenario where treatments have equal effects but differ across patterns for representative historical trial
      res_rate_mat_prior = rbind(
        c(0.3, 0.3, 0.3, 0.3),
        # pattern 1
        c(0.35, 0.35, 0.35, 0.35),
        # pattern 2
        c(0.40, 0.40, 0.40, 0.40),
        # pattern 3
        c(0.45, 0.45, 0.45, 0.45)    
        # pattern 4
      )
      
      # for scenario where treatments have equal effects but differ across patterns for unrepresentative historical trial 1
      res_rate_mat_prior_ur2 = rbind(
        c(0.2, 0.2, 0.2, 0.2),
        # pattern 1
        c(0.25, 0.25, 0.25, 0.25),
        # pattern 2
        c(0.3, 0.3, 0.3, 0.3),
        # pattern 3
        c(0.35, 0.35, 0.35, 0.35)     
        # pattern 4
      )
        
      # for scenario where treatments have equal effects but differ across patterns for unrepresentative historical trial 2 
      res_rate_mat_prior_ur1 = matrix(
        c(0.30, 0.35, 0.40, 0.45),
        byrow = TRUE,
        nrow = length(pattern_list),
        ncol = length(res_rate_prior_ur1)
      )
     } 
     else if ( scenario_name. %in% c("scenario4.2")) {
      # for scenario where treatment effects differ across patterns for current trial
      res_rate_mat = rbind(
        c(0.199, 0.250, 0.299, 0.350),
        # pattern 1
        c(0.291, 0.354, 0.413, 0.470),
        # pattern 2
        c(0.648, 0.711, 0.760, 0.799),
        # pattern 3
        c(0.833, 0.870, 0.896, 0.915)     
        # pattern 4
      )
      
      # for scenario where treatment effects differ across patterns for representative historical trial
      res_rate_mat_prior = rbind(
        c(0.199, 0.250, 0.299, 0.350),
        # pattern 1
        c(0.291, 0.354, 0.413, 0.470),
        # pattern 2
        c(0.648, 0.711, 0.760, 0.799),
        # pattern 3
        c(0.833, 0.870, 0.896, 0.915)    
        # pattern 4
      )
      
      # for scenario where treatment effects differ across patterns for unrepresentative historical trial 1
      res_rate_mat_prior_ur1 = matrix(
        c(0.20, 0.25, 0.30, 0.35),
        byrow = TRUE,
        nrow = length(pattern_list),
        ncol = length(res_rate_prior_ur1)
      )
      
      # for scenario where treatment effects differ across patterns for unrepresentative historical trial 2
      res_rate_mat_prior_ur2 = rbind(
        c(0.275, 0.275, 0.275, 0.275),
        # pattern 1
        c(0.383, 0.383, 0.383, 0.383),
        # pattern 2
        c(0.73, 0.73, 0.73, 0.73),
        # pattern 3
        c(0.879, 0.879, 0.879, 0.879)     
        # pattern 4
      )
     }
    }
    
    if (differsite > 0) { # in the scenarios where there are site effects 
      # make a matrix of pre-defined treatment effect per pattern for current trial for different sites
      res_rate_mat_site = matrix(
        T_vector-0.1,
        byrow = TRUE,
        nrow = length(pattern_list),
        # number of patterns
        ncol = length(T_vector)
      )
      
      # make a matrix of pre-defined treatment effect per pattern for representative historical trial for different sites
      res_rate_mat_prior_site = matrix(
        res_rate_prior-0.1,
        byrow = TRUE,
        nrow = length(pattern_list),
        ncol = length(res_rate_prior)
      )
      
      # make a matrix of pre-defined treatment effect per pattern for unrepresentative historical trial 1 for different sites
      res_rate_mat_prior_ur1_site = matrix(
        res_rate_prior_ur1-0.1,
        byrow = TRUE,
        nrow = length(pattern_list),
        ncol = length(res_rate_prior_ur1)
      )
      
      # make a matrix of pre-defined treatment effect per pattern for unrepresentative historical trial 2 for different sites
      res_rate_mat_prior_ur2_site = matrix(
        res_rate_prior_ur2-0.1,
        byrow = TRUE,
        nrow = length(pattern_list),
        ncol = length(res_rate_prior_ur2)
      )
    } 
    else {
      # make a matrix of pre-defined treatment effect per pattern which is equal for all sites
      res_rate_mat_site = res_rate_mat
      res_rate_mat_prior_site = res_rate_mat_prior
      res_rate_mat_prior_ur1_site = res_rate_mat_prior_ur1
      res_rate_mat_prior_ur2_site = res_rate_mat_prior_ur2
    }
    # run code to simulate data and do analysis
    scenario_out = simulation(
      N = N,
      N_hist = samplesize_hist,
      pattern = pattern_list,
      res_probability_prior = res_rate_mat_prior,
      res_probability_prior_ur1 = res_rate_mat_prior_ur1,
      res_probability_prior_ur2 = res_rate_mat_prior_ur2,
      res_probability_all = res_rate_mat,
      res_probability_prior_site = res_rate_mat_prior_site,
      res_probability_prior_ur1_site = res_rate_mat_prior_ur1_site,
      res_probability_prior_ur2_site = res_rate_mat_prior_ur2_site,
      res_probability_all_site = res_rate_mat_site,
      prob_pattern =  prob_pattern,
      differsite = differsite,
      R = N_iter,
      scenario_name = scenario_name
    ) # run N_iter iterations
    
    warning_data <- as.data.frame(rbind(scenario_out[[1]]$warn, scenario_out[[2]]$warn))
    
    message('simulation and models done...')
    
    save(warning_data, file = paste0("./Codes/Run_output/", scenario_name, "sample_size=", N, "_warnings.Rdata"))
    
    message(paste0(scenario_name, " warning data", " for sample_size = ", N, " saved in /Run_output"))
    
    error_reports <- as.data.frame(rbind(scenario_out[[1]]$errors, scenario_out[[2]]$errors))
    
    message(paste0(scenario_name, " error reports (if any)", " for sample_size = ", N, " saved in /Run_output"))
    
  
    # run code to summarise simulated outputs and produce estimands 
    analyse_out = process_sim_output(output_replication = scenario_out, 
                                     warning_data = warning_data, 
                                     error_reports = error_reports,
                                     T_v = T_vector,
                                     pattern = pattern_list,
                                     R = N_iter, 
                                     no_treatment = length(T_vector), 
                                     no_pattern = length(prob_pattern),
                                     lambda = prob_pattern,
                                     scenario_name = scenario_name)
    
    message('simulation outputs summarised...')
    
    store[[paste0('for size size = ', N)]] = list(scenario_out = scenario_out, 
                                                  analyse_out = analyse_out)
    
  }
  
  saveRDS(store,
          paste0("./Code/Run_output/scenario", scenario_name, '.rds'))
  
  message('simulation complete. output in `Run_output folder.`')
}

