###################################################### 
## Set up 
######################################################

# clean environment 
rm(list = ls())

# set working directory to the `practical/` folder 
wd = '~/Documents/GitHub/practical/'
setwd(wd)

# load libraries and functions
run_hpc <- FALSE

scripts = paste0(wd, 'Code/Functions/', list.files('Code/Functions/'))
lapply(scripts, source)

# if run hpc, need to reload the updated simulation.R function
# need to change the working directory in the simulation.R function

if(run_hpc){
  #library(crayon)
  library(parallel) # one of the core R packages
  library(doParallel)
  library(foreach)
  library(iterators)
  
  parallel::detectCores()
  #n.cores <- parallel::detectCores() - 1
  n.cores <- 7
  my.cluster <- parallel::makeCluster(
    n.cores
  )
  doParallel::registerDoParallel(cl = my.cluster)
  
  source(paste0(wd, 'Code/hpc/simulation.R'))
}


# set seed for reproducibility
set.seed(3127) 

# create an empty list to store timings
timings <- list()

#Specify sample size(s) for each run
N_patients_max = 200 # Max number of patients
N_patients_min = 100 # Min number of patients
N_patients_brk = 50 # Breaks within max and min number of patients

#Specify number of iterations for each scenario
No_iter = 2

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
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.375, 0.375, 0.375, 0.375),  # Treatment effects - first one being reference 
               res_rate_prior = (0.375, 0.375, 0.375, 0.375), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               scenario_name = '1.1'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 1.2
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.45, 0.35, 0.45, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.45, 0.35, 0.45, 0.45), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               scenario_name = '1.2'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 1.3
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.30, 0.35, 0.40, 0.45), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               scenario_name = '1.3'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 1.4
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.40, 0.50, 0.60),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.30, 0.40, 0.50, 0.60), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               scenario_name = '1.4'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
# S2 series: Low or high average treatment effects
######################################################
###################################################### 
## Run scenario 2.1
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.175, 0.175, 0.175, 0.175),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.175, 0.175, 0.175, 0.175), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               scenario_name = '2.1'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 2.2
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.10, 0.15, 0.20, 0.25),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.10, 0.15, 0.20, 0.25), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               scenario_name = '2.2'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 2.3
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.825, 0.825, 0.825, 0.825),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.825, 0.825, 0.825, 0.825), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               scenario_name = '2.3'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 2.4
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.75, 0.80, 0.85, 0.90),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.75, 0.80, 0.85, 0.90), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               scenario_name = '2.4'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
# S3 series: Different pattern prevalence
######################################################
###################################################### 
## Run scenario 3.1
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.10, P2 = 0.30, P3 = 0.30, P4 = 0.30), # Prevalence of each pattern
               T_vector = c(0.375, 0.375, 0.375, 0.375),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.375, 0.375, 0.375, 0.375), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               scenario_name = '3.1'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 3.2
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.10, P2 = 0.30, P3 = 0.30, P4 = 0.30), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.30, 0.35, 0.40, 0.45), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               scenario_name = '3.2'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
# S4 series: Different effects across patterns or sites
######################################################
###################################################### 
## Run scenario 4.1
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.20, 0.25, 0.30, 0.35),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.20, 0.25, 0.30, 0.35), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               pattsame = FALSE,    # If effects are the same across patterns
               differsite = 0,      # How many sites have different effects
               scenario_name = '4.1'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 4.2
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.30, 0.35, 0.40, 0.45), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               pattsame = FALSE,    # If effects are the same across patterns
               differsite = 2,      # How many sites have different effects
               scenario_name = '4.2'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 4.3
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.30, 0.35, 0.40, 0.45), # Priors
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               N_iter = No_iter,          # Number of iterations
               pattsame = FALSE,    # if effects are the same across patterns
               differsite = 4,      # how many sites have different effects
               scenario_name = '4.3'
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

######################################################
# Print timings for all scenarios
######################################################

names(timings) <- c("S1.1", 'S1.2', 'S1.3', 'S1,4', 
                    'S2.1', 'S2.2', 'S2.3', 'S2.4',
                    'S3.1', 'S3.2',
                    'S4.1', 'S4.2', 'S4.3')

cat("Timings for all scenarios:\n")
cat("Scenario 1.1 takes", timings$S1.1, "minutes\n")
cat("Scenario 1.2 takes", timings$S1.2, "minutes\n")
cat("Scenario 1.3 takes", timings$S1.3, "minutes\n")
cat("Scenario 1.4 takes", timings$S1.4, "minutes\n")
cat("Scenario 2.1 takes", timings$S2.1, "minutes\n")
cat("Scenario 2.2 takes", timings$S2.2, "minutes\n")
cat("Scenario 2.3 takes", timings$S2.3, "minutes\n")
cat("Scenario 2.4 takes", timings$S2.4, "minutes\n")
cat("Scenario 3.1 takes", timings$S3.1, "minutes\n")
cat("Scenario 3.2 takes", timings$S3.2, "minutes\n")
cat("Scenario 4.1 takes", timings$S4.1, "minutes\n")
cat("Scenario 4.2 takes", timings$S4.2, "minutes\n")
cat("Scenario 4.3 takes", timings$S4.3, "minutes\n")

saveRDS(timings,paste0("./Code/Run_output/timing.rds"))
