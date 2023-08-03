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



# Becky - 
# Yes I agree with your description of the way the model is implemented in R. 
# But the two models are equivalent, aren’t they?, 
# and the one implemented in R is a reparameterization of the model described in the paper, 
# so the results would be identical. 
# I think the way it’s described in the paper feels like a more natural way to explain the model in words. 

prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25) # Prevalence of each pattern
T_vector = c(0.9, 0.6, 0.3, 0.1)  # Treatment effects - first one being reference 
no_pattern <- length(pattern)
no_treatment <- length(unique(unlist(pattern)))
#res_probability_all<-matrix(rep(response_prob_V, no_pattern), ncol = no_treatment, byrow = T)
# response rate: row = pattern, column = treatment
colnames(res_probability_all) <-
  sapply(1:no_treatment, function(i) {
    paste0("treatment_", i)
  })
rownames(res_probability_all) <-
  sapply(1:no_pattern, function(i) {
    paste0("alpha_", i)
  })
# generate which pattern each patient in N patients belong to
# each person has prob_pattern to be allocated to one of the treatment patterns
assigned_pattern <- t(rmultinom(N, size = 1, prob_pattern))
colnames(assigned_pattern) <-
  sapply(1:no_pattern, function(i) {
    paste0("subgroup", i)
  })

# number of patients in each subgroup that is defined by the pattern
size_pattern <- apply(assigned_pattern, 2, sum)
lambda <- prob_pattern # true prevalence rate of patterns

sim_data = gen.data(
  no_pattern,
  size_pattern,
  pattern,
  res_probability_prior,
  res_probability_all,
  differsite
)

alldata = sim_data$trial_data
nma_data <- data.frame(y = unlist(alldata[1,]),
                       treatment = factor(unlist(alldata[2,]), levels = sort(unique(unlist(alldata[2,])))),
                       subgroup = factor(unlist(alldata[4,])))
becky <- glm(y ~ treatment + subgroup, family = "binomial", data = nma_data)

my.glm <- glmer(y ~ treatment + (1 | subgroup), family = "binomial", data = nma_data)
