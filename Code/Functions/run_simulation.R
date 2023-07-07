## function to run one scenario

run_simulation <- function(no_treatment,   # No. of treatments within simulation
                           pattern_list,   # treatment patterns
                           prob_pattern,   # prevalence of each pattern
                           T_vector,       # Treatment effects - first one being baseline 
                           res_rate_prior, # Priors
                           N_patients_max, # Max number of patients
                           N_patients_min, # Max number of patients
                           N_patients_brk, # Breaks within max and min number of patients
                           N_iter,         # Number of iterations
                           alpha = 0,
                           scenario_name,
                           differsite,   ###sites we want modify treatment effects 
                           differpattern ### patterns we want to modify treatment effects 
){
  
  #Specify each treatment risk
  alpha_ref = find_phi(T_vector[1], alpha)   # effect of reference treatment 
  phi_vector = find_phi(p = T_vector, 
                        alpha = alpha_ref)   # specify each treatment risk
  res_rate = res_probability(phi_vector, alpha_ref) # probability of outcome for each treatment 
  
  #### res_rate is just the same as T_vector?
  
  #Specify sample size for each run
  samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk)
  
  store = list()
  for (N in samplesize_vec) {
    
    message(paste0('Starting simulation for sample size = ', N))
    
    res_rate_mat = matrix(res_rate, byrow = T,
                          nrow = length(pattern_list), # number of patterns
                          ncol = length(res_rate)) # number of treatments
    
    res_rate_mat_prior = matrix(res_rate_prior, byrow = T,
                                nrow= length(pattern_list), 
                                ncol = length(res_rate))

     for (p in 1:length(pattern_list)){
      for (d in 1: length(differpattern))
      if(p == differpattern[[d]][1]){
        res_rate_mat[p,] <- res_rate_mat[p,] * differpattern[[d]][2]  
        res_rate_mat_prior[p,] <- res_rate_mat_prior[p,] * differpattern[[d]][2] 
      }
    }
    
    scenario_out = simulation(N = N, # for N patients 
                              phi_v = phi_vector, 
                              pattern = pattern_list, 
                              res_probability_prior = res_rate_mat_prior,
                              res_probability_all = res_rate_mat,
                              prob_pattern = prob_pattern, 
                              R = N_iter, differsite = differsite) # run N_iter iterations
    
    store [[paste0('for size size = ', N)]] = scenario_out
    
  }
  
  saveRDS(store, paste0("./Code/Run_output/scenario", scenario_name, '.rds'))
  
  message('simulation complete. output in `Run_output folder.`')
}
