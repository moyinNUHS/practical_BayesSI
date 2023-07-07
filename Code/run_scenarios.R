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

###################################################### 
## Run scenario 1
######################################################

run_simulation(no_treatment = 4,                      # No. of treatments within simulation
               pattern_list = list(pattern1 = c(2,3), # Treatment patterns
                                   pattern2 = 1:3,
                                   pattern3 = c(2,3,4),
                                   pattern4 = c(1,2,3,4)),   
               prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25),   # prevalence of each pattern
               T_vector = c(0.35, 0.35, 0.35, 0.35),       # Treatment effects - first one being baseline 
               res_rate_prior = c(0.35, 0.35, 0.35, 0.35), # Priors
               N_patients_max = 100, # Max number of patients
               N_patients_min = 50, # Max number of patients
               N_patients_brk = 10, # Breaks within max and min number of patients
               N_iter = 5,          # Number of iterations
               alpha = 0,
               scenario_name = '1',
               differsite = list(c(2, 1.1), c(4, .99)), ## the sites we want to modify treatment effect for and the coefficient of modification 
               differpattern = list(c(2, .95)) ##the pattern for which we want to modify effect values and the coefficient of modification
)

###################################################### 
## Run scenario 2
######################################################

