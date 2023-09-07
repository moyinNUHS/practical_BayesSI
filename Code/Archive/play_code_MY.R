## RUN SCENARIO

prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25) # Prevalence of each pattern
T_vector = c(0.9, 0.6, 0.3, 0.1)  # Treatment effects - first one being reference 
res_rate_prior = c(0.375, 0.375, 0.375, 0.375) # Priors
samplesize_vec = seq(400, 500, by = 50) #Sample size for each simulation
N_iter = 2

## RUN SIMULATION
pattern_list = list( # Treatment patterns
  pattern1 = c(2, 3), 
  pattern2 = c(1, 2, 3),
  pattern3 = c(2, 3, 4),
  pattern4 = c(1, 2, 3, 4))
pattsame = TRUE
differsite = 0

alpha_ref = find_phi(p = T_vector[1], alpha = 0)   # reference treatment effect 
phi_vector = find_phi(p = T_vector, alpha = alpha_ref) # specify each treatment effect in terms of OR

res_rate_mat = matrix(
  T_vector,
  byrow = T,
  nrow = length(pattern_list),
  # number of patterns
  ncol = length(T_vector)
)    

res_rate_mat_prior = matrix(
  res_rate_prior,
  byrow = T,
  nrow = length(pattern_list),
  ncol = length(res_rate_prior)
)

N = 1000
# for N patients
phi_v = phi_vector
pattern = pattern_list
res_probability_prior = res_rate_mat_prior
res_probability_all = res_rate_mat
prob_pattern = prob_pattern
differsite = differsite
R = N_iter

# SIMULATION 

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

k = 2

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

# generate prior historical data 
# put historical data in a dataframe - outcome, treatment, pattern/subgroup
nma_data_prior <- data.frame(
  y = unlist(sim_data[['prior_data']][1,]),
  treatment = factor(unlist(sim_data[['prior_data']][2,]), levels = sort(unique(
    unlist(sim_data[['prior_data']][2,])
  ))),
  subgroup = factor(unlist(sim_data[['prior_data']][4,]))#,
  #site=factor(unlist(sim_data[['prior_data']][5,]))
)

# generate current data 
# put current data in a dataframe - outcome, treatment, pattern/subgroup
nma_data <- data.frame(
  y = unlist(sim_data[['trial_data']][1,]),
  treatment = factor(unlist(sim_data[['trial_data']][2,]), levels = sort(unique(
    unlist(sim_data[['trial_data']][2,])
  ))),
  subgroup = factor(unlist(sim_data[['trial_data']][4,]))#,
  #site=factor(unlist(sim_data[['trial_data']][5,]))
)


alldata = sim_data[['trial_data']]
alt_hypothesis = 'two.sided'
bonferr = T
dunnett = F
p = 0.05

Scale_wk <- 5     #Larger scale assigns less importance to prior distribution
Scale_str <- 1    #Smaller scale assigns more importance to prior distribution
