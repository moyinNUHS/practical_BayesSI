prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25) # Prevalence of each pattern
T_vector = c(0.9, 0.6, 0.3, 0.1)  # Treatment effects - first one being reference 
res_rate_prior = c(0.9, 0.6, 0.3, 0.1) # Priors
N = 300 # Max number of patients
N_iter = 1          # Number of iterations
no_treatment = 4
# No. of treatments within simulation
pattern_list = list( # Treatment patterns
  pattern1 = c(2, 3), 
  pattern2 = c(1, 2, 3), 
  pattern3 = c(2, 3, 4),
  pattern4 = c(1, 2, 3, 4))
differsite=0

#Specify each treatment risk
alpha_ref = find_phi(p = T_vector[1], alpha = 0)   # reference treatment effect 
phi_vector = find_phi(p = T_vector, alpha = alpha_ref) # specify each treatment effect in terms of OR
# you may get back T_vector with res_probability(phi_vector, alpha_ref)

#Specify sample size for each run
res_rate_mat = matrix(
  T_vector,
  byrow = T,
  nrow = length(pattern_list),
  # number of patterns
  ncol = length(T_vector)
)     # number of treatments
res_rate_mat_prior = matrix(
  res_rate_prior,
  byrow = T,
  nrow = length(pattern_list),
  ncol = length(res_rate_prior)
)

phi_v = phi_vector
pattern = pattern_list
res_probability_prior = res_rate_mat_prior
res_probability_all = res_rate_mat
prob_pattern = prob_pattern
R = N_iter

alt_hypothesis = 'two.sided' 
type1correction = T

# correct answer should be 3, 3, 4, 4 