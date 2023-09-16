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
  n.cores <- 100
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
N_patients_max = 2000 # Max number of patients
N_patients_min = 500 # Min number of patients
N_patients_brk = 500 # Breaks within max and min number of patients

#Specify sample size of historical clinical trial
N_hist=500
#Specify number of iterations for each scenario
No_iter = 500
#########
# Note
#########
#`run_simulation` function is fixed with the following conditions as we are not exploring them: 
# no_treatment = 4
# pattern_list = list(pattern1 = c(2,3), pattern2 = 1:3, pattern3 = c(2,3,4), pattern4 = c(1,2,3,4)),

# boundary (singular) fit: see help('isSingular') error message indicates that 
# the model did fit but random effects are very small

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
               res_rate_prior_ur1 = c(0.325, 0.325, 0.325, 0.325), # Priors ur1
               res_rate_prior_ur2 = c(0.3,0.35,0.4,0.45), # Priors ur2
               samplesize_vec = seq(N_patients_min, N_patients_max, by = N_patients_brk), #Sample size for each simulation
               samplesize_hist = N_hist,
               N_iter = No_iter,          # Number of iterations
               scenario_name = paste0(c('scenario3.1', paste0('iter', No_iter), as.character(Sys.Date())), collapse = '_')
)
end_time <- Sys.time()
time_taken <- end_time - start_time

saveRDS(time_taken,paste0("./Code/Run_output/timing_3.1.rds"))
parallel::stopCluster(my.cluster)