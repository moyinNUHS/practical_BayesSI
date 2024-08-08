#.libPaths("/data/chakraborty/home/e0859927/R/rstudio/4.1")


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
N_SS_vec = c(500, 1000, 1500, 2000, 3000, 4000, 5000)

#Specify sample size of historical clinical trial
N_hist=500

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
               T_vector = c(0.375, 0.375, 0.375, 0.375),  # Treatment effects - % mortality 
               res_rate_prior = c(0.375, 0.375, 0.375, 0.375), # Priors
               res_rate_prior_ur1 = c(0.275, 0.275, 0.275, 0.275), # Priors ur1
               res_rate_prior_ur2 = c(0.3, 0.35, 0.4, 0.45), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario1.1', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 1.2
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.45, 0.35, 0.45, 0.45),  # Treatment effects 
               res_rate_prior = c(0.45, 0.35, 0.45, 0.45), # Priors
               res_rate_prior_ur1 = c(0.35, 0.25, 0.35, 0.35), # Priors ur1
               res_rate_prior_ur2 = c(0.45, 0.4, 0.35, 0.3), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario1.2', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 1.3
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects 
               res_rate_prior = c(0.30, 0.35, 0.40, 0.45), # Priors
               res_rate_prior_ur1 = c(0.20, 0.25, 0.30, 0.35), # Priors ur1
               res_rate_prior_ur2 = c(0.375, 0.375, 0.375, 0.375), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario1.3', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 1.5
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.40, 0.50, 0.60),  # Treatment effects 
               res_rate_prior = c(0.30, 0.40, 0.50, 0.60), # Priors
               res_rate_prior_ur1 = c(0.20, 0.30, 0.40, 0.50), # Priors ur1
               res_rate_prior_ur2 = c(0.45, 0.45, 0.45, 0.45), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario1.5', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 1.6
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.35, 0.30, 0.45, 0.40),  # Treatment effects 
               res_rate_prior = c(0.35, 0.30, 0.45, 0.40), # Priors
               res_rate_prior_ur1 = c(0.25, 0.20, 0.35, 0.30), # Priors ur1
               res_rate_prior_ur2 = c(0.375, 0.375, 0.375, 0.375), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario1.6', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 1.4
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.35, 0.45, 0.35,  0.35),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.35, 0.45, 0.35,  0.35), # Priors
               res_rate_prior_ur1 = c(0.25, 0.25, 0.25, 0.25), # Priors ur1
               res_rate_prior_ur2 = c(0.45,0.4,0.35,0.3), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario1.4', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
# S3 series: Low or high average treatment effects
######################################################
###################################################### 
## Run scenario 3.1
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.175, 0.175, 0.175, 0.175),  # Treatment effects 
               res_rate_prior = c(0.175, 0.175, 0.175, 0.175), # Priors
               res_rate_prior_ur1 = c(0.075, 0.075, 0.075, 0.075), # Priors ur1
               res_rate_prior_ur2 = c(0.10, 0.15, 0.20, 0.25), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario3.1', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 3.2
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.10, 0.15, 0.20, 0.25),  # Treatment effects
               res_rate_prior = c(0.10, 0.15, 0.20, 0.25), # Priors
               res_rate_prior_ur1 = c(0.05, 0.10, 0.15, 0.20), # Priors ur1
               res_rate_prior_ur2 = c(0.175, 0.175, 0.175, 0.175), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario3.2', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 3.3
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.825, 0.825, 0.825, 0.825),  # Treatment effects 
               res_rate_prior = c(0.825, 0.825, 0.825, 0.825), # Priors
               res_rate_prior_ur1 = c(0.725, 0.725, 0.725, 0.725), # Priors ur1
               res_rate_prior_ur2 = c(0.75, 0.80, 0.85, 0.90), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario3.3', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 3.4
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.75, 0.80, 0.85, 0.90),  # Treatment effects 
               res_rate_prior = c(0.75, 0.80, 0.85, 0.90), # Priors
               res_rate_prior_ur1 = c(0.65, 0.70, 0.75, 0.80), # Priors ur1
               res_rate_prior_ur2 = c(0.825, 0.825, 0.825, 0.825), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario3.4', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
# S4 series: Different pattern prevalence
######################################################
###################################################### 
## Run scenario 4.1
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.10, P2 = 0.30, P3 = 0.30, P4 = 0.30), # Prevalence of each pattern
               T_vector = c(0.375, 0.375, 0.375, 0.375),  # Treatment effects 
               res_rate_prior = c(0.375, 0.375, 0.375, 0.375), # Priors
               res_rate_prior_ur1 = c(0.275, 0.275, 0.275, 0.275), # Priors ur1
               res_rate_prior_ur2 = c(0.30, 0.35, 0.40, 0.45), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario4.1', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 4.2
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.10, P2 = 0.30, P3 = 0.30, P4 = 0.30), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects 
               res_rate_prior = c(0.30, 0.35, 0.40, 0.45), # Priors
               res_rate_prior_ur1 = c(0.20, 0.25, 0.30, 0.35), # Priors ur1
               res_rate_prior_ur2 = c(0.375, 0.375, 0.375, 0.375), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario4.2', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
# S2 series: Different effects across patterns or sites
######################################################
###################################################### 
## Run scenario 2.1
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.30, 0.30, 0.30),  # Treatment effects 
               res_rate_prior = c(0.30, 0.30, 0.30, 0.30), # Priors
               res_rate_prior_ur1 = c(0.20, 0.20, 0.20, 0.20), # Priors ur1
               res_rate_prior_ur2 = c(0.30, 0.35, 0.40, 0.45), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               pattsame = FALSE,    # If effects are the same across patterns
               differsite = 0,      # How many sites have different effects
               scenario_name = paste0(c('scenario2.1', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 2.2
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.199, 0.25, 0.299, 0.35),  # Treatment effects 
               res_rate_prior = c(0.199, 0.25, 0.299, 0.35), # Priors
               res_rate_prior_ur1 = c(0.20, 0.20, 0.20, 0.20), # Priors ur1
               res_rate_prior_ur2 = c(0.275, 0.275, 0.275, 0.275), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               pattsame = FALSE,    # If effects are the same across patterns
               differsite = 0,      # How many sites have different effects
               scenario_name = paste0(c('scenario2.2', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

###################################################### 
## Run scenario 2.3
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.30, 0.35, 0.40, 0.45),  # Treatment effects 
               res_rate_prior = c(0.30, 0.35, 0.40, 0.45), # Priors
               res_rate_prior_ur1 = c(0.20, 0.25, 0.30, 0.35), # Priors ur1
               res_rate_prior_ur2 = c(0.375, 0.375, 0.375, 0.375), # Priors ur2
               samplesize_vec = N_SS_vec, #Sample size for each simulation
               samplesize_hist = N_hist, #Sample size of historical trial
               N_iter = No_iter,          # Number of iterations
               pattsame = TRUE,    # If effects are the same across patterns
               differsite = 2,      # How many sites have different effects
               scenario_name = paste0(c('scenario2.3', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)

end_time <- Sys.time()
time_taken <- end_time - start_time
timings <- append(timings, as.numeric(time_taken))

######################################################
# Print timings for all scenarios
######################################################

names(timings) <- c("S1.1", 'S1.2', 'S1.3', 'S1.4', 'S1.5', 'S1.6', 
                    'S2.1', 'S2.2', 'S2.3', 
                    'S3.1', 'S3.2', 'S3.3', 'S3.4',
                    'S4.1', 'S4.2')

cat("Timings for all scenarios:\n")
cat("Scenario 1.1 takes", timings$S1.1, "minutes\n")
cat("Scenario 1.2 takes", timings$S1.2, "minutes\n")
cat("Scenario 1.3 takes", timings$S1.3, "minutes\n")
cat("Scenario 1.4 takes", timings$S1.4, "minutes\n")
cat("Scenario 1.5 takes", timings$S1.5, "minutes\n")
cat("Scenario 1.6 takes", timings$S1.6, "minutes\n")
cat("Scenario 2.1 takes", timings$S2.1, "minutes\n")
cat("Scenario 2.2 takes", timings$S2.2, "minutes\n")
cat("Scenario 2.3 takes", timings$S2.2, "minutes\n")
cat("Scenario 3.1 takes", timings$S3.1, "minutes\n")
cat("Scenario 3.2 takes", timings$S3.2, "minutes\n")
cat("Scenario 3.3 takes", timings$S2.3, "minutes\n")
cat("Scenario 3.4 takes", timings$S2.4, "minutes\n")
cat("Scenario 4.1 takes", timings$S4.1, "minutes\n")
cat("Scenario 4.2 takes", timings$S4.2, "minutes\n")

saveRDS(timings,paste0("./Code/Run_output/timing.rds"))
