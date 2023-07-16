###################################################### 
## Set up 
######################################################

# clean environment 
rm(list = ls())

# set working directory to the `practical/` folder 
wd = '~/Documents/GitHub/practical/'
setwd(wd)

# load libraries and functions
scripts = paste0(wd, 'Code/Functions/', list.files('Code/Functions/'))
lapply(scripts, source)

# set seed for reproducibility
set.seed(3127) 

#########
# Note
#########
#`run_simulation` function is fixed with the following conditions as we are not exploring them: 
# no_treatment = 4
# pattern_list = list(pattern1 = c(2,3), pattern2 = 1:3, pattern3 = c(2,3,4), pattern4 = c(1,2,3,4)),

# boundary (singular) fit: see help('isSingular') error message indicates that 
# the model did fit but random effects are very small

###################################################### 
# S1 series: Various effects across patterns
######################################################
###################################################### 
## Run scenario 1.1
######################################################

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.375, 0.375, 0.375, 0.375),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '1.1'
)


###################################################### 
## Run scenario 1.2
######################################################

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.45, 0.35, 0.45, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '1.2'
)

###################################################### 
## Run scenario 1.3
######################################################

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '1.3'
)

###################################################### 
## Run scenario 1.4
######################################################

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.40, 0.50, 0.60),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '1.4'
)

###################################################### 
# S2 series: Low or high average treatment effects
######################################################
###################################################### 
## Run scenario 2.1
######################################################

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.175, 0.175, 0.175, 0.175),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '2.1'
)

###################################################### 
## Run scenario 2.2
######################################################

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.10, 0.15, 0.20, 0.25),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '2.2'
)

###################################################### 
## Run scenario 2.3
######################################################

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.825, 0.825, 0.825, 0.825),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '2.3'
)

###################################################### 
## Run scenario 2.4
######################################################

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.75, 0.80, 0.85, 0.90),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '2.4'
)

###################################################### 
# S3 series: Different pattern prevalence
######################################################
###################################################### 
## Run scenario 3.1
######################################################

run_simulation(prob_pattern = c(P1 = 0.10, P2 = 0.30, P3 = 0.30, P4 = 0.30), # Prevalence of each pattern
               T_vector = c(0.375, 0.375, 0.375, 0.375),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '3.1'
)

###################################################### 
## Run scenario 3.2
######################################################

run_simulation(prob_pattern = c(P1 = 0.10, P2 = 0.30, P3 = 0.30, P4 = 0.30), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '3.2'
)

###################################################### 
# S4 series: Different effects across patterns or sites
######################################################
###################################################### 
## Run scenario 4.1
######################################################
