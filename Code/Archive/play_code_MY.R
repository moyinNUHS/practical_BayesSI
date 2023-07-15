prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25) # Prevalence of each pattern
T_vector = c(0.9, 0.6, 0.3, 0.1)  # Treatment effects - first one being reference 
res_rate_prior = c(0.9, 0.6, 0.3, 0.1) # Priors
N = 1500 # Max number of patients
N_iter = 5          # Number of iterations
no_treatment = 4
# No. of treatments within simulation
pattern_list = list( # Treatment patterns
  pattern1 = c(2, 3), 
  pattern2 = c(1, 2, 3), 
  pattern3 = c(2, 3, 4),
  pattern4 = c(1, 2, 3, 4))
differsite=0
phi_v = phi_vector
pattern = pattern_list
res_probability_prior = res_rate_mat_prior
res_probability_all = res_rate_mat
prob_pattern = prob_pattern
R = N_iter

# correct answer should be 3, 3, 4, 4 