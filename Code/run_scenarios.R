library(tictoc)

###################################################### 
## Set up 
######################################################

# clean environment 
rm(list = ls())

# set working directory to the `practical/` folder 
wd = 'C:/Users/chuaj/OneDrive/Desktop/practical'
setwd(wd)

# load libraries and functions
scripts = paste0(wd, '/Code/Functions/', list.files('Code/Functions/'))
lapply(scripts, source)

# set seed for reproducibility
set.seed(3127) 

# Create an empty list to store timings
timings <- list()

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
tic('timer1.1')

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.375, 0.375, 0.375, 0.375),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '1.1'
)

toc()
timings$scenario1.1 <- toc()

###################################################### 
## Run scenario 1.2
######################################################
tic('timer1.2')

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.45, 0.35, 0.45, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '1.2'
)

toc()
timings$scenario1.2 <- toc()

###################################################### 
## Run scenario 1.3
######################################################
tic('timer1.3')

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '1.3'
)

toc()
timings$scenario1.3 <- toc()

###################################################### 
## Run scenario 1.4
######################################################
tic('timer1.4')

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.40, 0.50, 0.60),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '1.4'
)

toc()
timings$scenario1.4 <- toc()

###################################################### 
# S2 series: Low or high average treatment effects
######################################################
###################################################### 
## Run scenario 2.1
######################################################
tic('timer2.1')

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.175, 0.175, 0.175, 0.175),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '2.1'
)

toc()
timings$scenario2.1 <- toc()

###################################################### 
## Run scenario 2.2
######################################################
tic('timer2.2')

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.10, 0.15, 0.20, 0.25),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '2.2'
)

toc()
timings$scenario2.2 <- toc()

###################################################### 
## Run scenario 2.3
######################################################
tic('timer2.3')

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.825, 0.825, 0.825, 0.825),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '2.3'
)

toc()
timings$scenario2.3 <- toc()

###################################################### 
## Run scenario 2.4
######################################################
tic('timer2.4')

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.75, 0.80, 0.85, 0.90),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '2.4'
)

toc()
timings$scenario2.4 <- toc()

###################################################### 
# S3 series: Different pattern prevalence
######################################################
###################################################### 
## Run scenario 3.1
######################################################
tic('timer3.1')

run_simulation(prob_pattern = c(P1 = 0.10, P2 = 0.30, P3 = 0.30, P4 = 0.30), # Prevalence of each pattern
               T_vector = c(0.375, 0.375, 0.375, 0.375),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '3.1'
)

toc()
timings$scenario3.1 <- toc()

###################################################### 
## Run scenario 3.2
######################################################
tic('timer3.2')

run_simulation(prob_pattern = c(P1 = 0.10, P2 = 0.30, P3 = 0.30, P4 = 0.30), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               scenario_name = '3.2'
)

toc()
timings$scenario3.2 <- toc()

###################################################### 
# S4 series: Different effects across patterns or sites
######################################################
###################################################### 
## Run scenario 4.1
######################################################
tic('timer4.1')

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.20, 0.25, 0.30, 0.35),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               pattsame = FALSE,    # If effects are the same across patterns
               differsite = 0,      # How many sites have different effects
               scenario_name = '4.1'
)

toc()
timings$scenario4.1 <- toc()

###################################################### 
## Run scenario 4.2
######################################################
tic('timer4.2')

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               pattsame = FALSE,    # If effects are the same across patterns
               differsite = 2,      # How many sites have different effects
               scenario_name = '4.2'
)

toc()
timings$scenario4.2 <- toc()

###################################################### 
## Run scenario 4.3
######################################################
tic('timer4.3')

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.9, 0.7, 0.5, 0.1), # Priors
               N_patients_max = 200, # Max number of patients
               N_patients_min = 100, # Max number of patients
               N_patients_brk = 50, # Breaks within max and min number of patients
               N_iter = 2,          # Number of iterations
               pattsame = FALSE,    # if effects are the same across patterns
               differsite = 4,      # how many sites have different effects
               scenario_name = '4.3'
)

toc()
timings$scenario4.3 <- toc()

######################################################


######################################################
# Print timings for all scenarios
######################################################
cat("Timings for all scenarios:\n")
cat("Scenario 1.1 takes", timings$scenario1.1, "seconds\n")
cat("Scenario 1.2 takes", timings$scenario1.2, "seconds\n")
cat("Scenario 1.3 takes", timings$scenario1.3, "seconds\n")
cat("Scenario 1.4 takes", timings$scenario1.4, "seconds\n")
cat("Scenario 2.1 takes", timings$scenario2.1, "seconds\n")
cat("Scenario 2.2 takes", timings$scenario2.2, "seconds\n")
cat("Scenario 2.3 takes", timings$scenario2.3, "seconds\n")
cat("Scenario 2.4 takes", timings$scenario2.4, "seconds\n")
cat("Scenario 3.1 takes", timings$scenario3.1, "seconds\n")
cat("Scenario 3.2 takes", timings$scenario3.2, "seconds\n")
cat("Scenario 4.1 takes", timings$scenario4.1, "seconds\n")
cat("Scenario 4.2 takes", timings$scenario4.2, "seconds\n")
cat("Scenario 4.3 takes", timings$scenario4.3, "seconds\n")
