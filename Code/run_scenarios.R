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

###################################################### 
## Run scenario 1.1
######################################################

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.9, 0.7, 0.5, 0.1),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '1.1'
)


###################################################### 
## Run scenario 2
######################################################

