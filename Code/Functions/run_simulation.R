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
                           samplesize_vec, 
                           #Sample size for each simulation
                           N_iter,
                           # Number of iterations
                           pattsame = TRUE,
                           # if effects are the same across patterns,
                           differsite = 0,
                           #  how many sites have different effects,
                           scenario_name) {
  
  #Specify each treatment risk
  alpha_ref = find_phi(p = T_vector[1], alpha = 0)   # reference treatment effect 
  phi_vector = find_phi(p = T_vector, alpha = alpha_ref) # specify each treatment effect in terms of OR
  # you may get back T_vector with res_probability(phi_vector, alpha_ref)
  
  store = list()
  for (N in samplesize_vec) {
    message(paste0('Starting simulation for sample size = ', N))
    
    if (pattsame) {
      # make a matrix of pre-defined treatment effect per pattern
      res_rate_mat = matrix(
        T_vector,
        byrow = T,
        nrow = length(pattern_list),
        # number of patterns
        ncol = length(T_vector)
      )     # number of treatments
    } else {
      # for scenario where treatment effects differ across patterns
      res_rate_mat = rbind(
        c(0.2, 0.25, 0.3, 0.35),
        # pattern 1
        c(0.2, 0.3, 0.4, 0.5),
        # pattern 2
        c(0.4, 0.6, 0.75, 0.95),
        # pattern 3
        c(0.3, 0.4, 0.8, 0.9)     
        # pattern 4
      )
    }

    if (pattsame) {
      # make a matrix of prior treatment effect per pattern
    res_rate_mat_prior = matrix(
      res_rate_prior,
      byrow = T,
      nrow = length(pattern_list),
      ncol = length(res_rate_prior)
    )
    } else {
      # for scenario where treatment effects differ across patterns
      res_rate_mat_prior = rbind(
        c(0.2, 0.25, 0.3, 0.35),
        # pattern 1
        c(0.2, 0.3, 0.4, 0.5),
        # pattern 2
        c(0.4, 0.6, 0.75, 0.95),
        # pattern 3
        c(0.3, 0.4, 0.8, 0.9)     
        # pattern 4
      )
    }
    
    # run code to simulate data and do analysis
    scenario_out = simulation(
      N = N,
      # for N patients
      phi_v = phi_vector,
      pattern = pattern_list,
      res_probability_prior = res_rate_mat_prior,
      res_probability_all = res_rate_mat,
      prob_pattern = prob_pattern,
      differsite = differsite,
      R = N_iter
    ) # run N_iter iterations
    
    message('simulation and models done...')
    
    # run code to summarise simulated outputs and produce estimands 
    analyse_out = process_sim_output(output_replication = scenario_out, 
                                     phi_v = phi_vector,
                                     pattern = pattern_list,
                                     R = N_iter, 
                                     no_treatment = length(phi_vector), 
                                     no_pattern = length(prob_pattern),
                                     lambda = prob_pattern)
    # for each pattern, the number of pairwise comparisons fixing a reference treatment)
    
    message('simulation outputs summarised...')
    
    store[[paste0('for size size = ', N)]] = list(scenario_out = scenario_out, 
                                                   analyse_out = analyse_out)
    
  }
  
  saveRDS(store,
          paste0("./Code/Run_output/scenario", scenario_name, '.rds'))
  
  message('simulation complete. output in `Run_output folder.`')
}
