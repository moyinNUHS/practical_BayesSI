.libPaths("/data/chakraborty/home/e0859927/R/rstudio/4.1")

###################################################### 
## Set up 
######################################################

# clean environment 
rm(list = ls())

# set working directory to the `practical/` folder 
wd = '/data/chakraborty/home/e0859927/practical/'
setwd(wd)

# load libraries and functions
run_hpc <- TRUE

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
  n.cores <- 50
  my.cluster <- parallel::makeCluster(
    n.cores,
    type = "SOCK"
  )
  doParallel::registerDoParallel(cl = my.cluster)
  
  source(paste0(wd, 'Code/hpc/simulation.R'))
}


# set seed for reproducibility
set.seed(3127) 

# create an empty list to store timings
timings <- list()

#Specify sample size(s) for each run
N_patients_max = 2300 # Max number of patients
N_patients_min = 300 # Min number of patients
N_patients_brk = 500 # Breaks within max and min number of patients

#Specify sample size of historical clinical trial
N_hist=500
#Specify number of iterations for each scenario
No_iter = 100
#########
# Note
#########
#`run_simulation` function is fixed with the following conditions as we are not exploring them: 
# no_treatment = 4
# pattern_list = list(pattern1 = c(2,3), pattern2 = 1:3, pattern3 = c(2,3,4), pattern4 = c(1,2,3,4)),

# boundary (singular) fit: see help('isSingular') error message indicates that 
# the model did fit but random effects are very small

###################################################### 
# S2 series: Low or high average treatment effects
######################################################
###################################################### 
## Run scenario 2.3
######################################################
start_time <- Sys.time()

run_simulation(prob_pattern = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25), # Prevalence of each pattern
               T_vector = c(0.825, 0.825, 0.825, 0.825),  # Treatment effects - first one being reference 
               res_rate_prior = c(0.825, 0.825, 0.825, 0.825), # Priors
               res_rate_prior_ur1 = c(0.725,0.725,0.725,0.725), # Priors ur1
               res_rate_prior_ur2 = c(0.75,0.80,0.85,0.90), # Priors ur2
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               samplesize_hist = N_hist,
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario2.3', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)
end_time <- Sys.time()
time_taken <- end_time - start_time

saveRDS(time_taken,paste0("./Code/Run_output/timing_2.3.rds"))
parallel::stopCluster(my.cluster)