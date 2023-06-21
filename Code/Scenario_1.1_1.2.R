#Run Simulation for current scenarios
#Have included scenarios 1.1 and 1.2, and included a loop to show how we could produce results for multiple scenarios in one script. 
#However, having separate scripts per scenario may be better/easier/quicker?
#Alternatively, could loop by sample size instead? I include another loop by sample size as an example
#Also include example of how scenario 1.4 can be coded

#Load Packages
library(stringr)
library(multiwayvcov)
require(sandwich)
library(rstanarm)
library(modelsummary)
library(multilevelmod)
library(tidymodels)

#Load Functions               #FYI unsure how to source functions from a different folder within github?
source("find.rankings.R")
source("find_phi.R")
source("fit_onestage_C.R")
source("fit_onestage_C_NI.R")
source("fit_onestage_C_wk.R")
source("fit_onestage_C_str.R")
source("fit_robustSE_D.R")
source("generate_subset_data.R")
source("myTryCatch.R")
source("res_probability.R")
source("simulation.R")

#Set seed for reproducability
set.seed(3127) 

#No. of treatments within simulation
no_treatment=4 

# treatment patterns
pattern1<-c(2,3)
pattern2<-1:3
pattern3<-c(2,3,4)
pattern4<-c(1,2,3,4)
patternV<-list(pattern1, pattern2, pattern3, pattern4)
pattern<-patternV

#Prevalence of each pattern
P1<-0.25
P2<-0.25
P3<-0.25
P4<-0.25
prob_pattern= c(P1, P2, P3, P4) 

#### scenario 1.1
# current treatment effect parameters
baseline<-0.35    #Baseline treatment effect
T1<-baseline
T2<-baseline
T3<-baseline
T4<-baseline
T_current<-c(T1, T2, T3, T4)

#Specify each treatment risk
alpha_1 <- find_phi(baseline, alpha = 0)
phi_1 <- find_phi(p = T_current, 
                  alpha = alpha_1) #specify each treatment risk
res_rate1<-res_probability(phi_1,alpha_1)

# prior treatment effect parameters (matches current treatment effect exactly)
baseline_p<-0.35    #Baseline treatment effect
T1_p<-baseline
T2_p<-baseline
T3_p<-baseline
T4_p<-baseline
T_prior<-c(T1_p, T2_p, T3_p, T4_p)

#Specify each treatment prior risk (representing historical data)
alpha_1.p <- find_phi(baseline_p, alpha = 0)
phi_1.p <- find_phi(p = T_prior, 
                  alpha = alpha_1.p) #specify each treatment risk
res_rate1.p<-res_probability(phi_1.p,alpha_1.p)

#### scenario 1.2
# current treatment effect parameters
T1<-0.6
T2<-baseline
T3<-baseline
T4<-baseline
T_current<-c(T1, T2, T3, T4)

#Specify each treatment risk
phi_1.1<-find_phi(p = T_current, 
                  alpha = alpha_1) #specify each treatment risk
res_rate1.1<-res_probability(phi_1.1,alpha_1)

# prior treatment effect parameters (matches current treatment effect exactly)
T1_p<-0.6
T2_p<-baseline
T3_p<-baseline
T4_p<-baseline
T_prior<-c(T1_p, T2_p, T3_p, T4_p)

#Specify each treatment prior risk (representing historical data)
phi_1.1.p <- find_phi(p = T_prior, 
                  alpha = alpha_1.p) #specify each treatment risk
res_rate1.1.p<-res_probability(phi_1.1.p,alpha_1.p)

#### scenario 1.4
baseline<-0.35
T_current<-seq(0.45, 0.3, length.out = no_treatment)

#Specify each treatment risk
alpha_1.4 <- find_phi(baseline, alpha = 0)
phi_1.4<-find_phi(p = T_current, 
                  alpha = alpha_1) #specify each treatment risk
res_rate1.4<-res_probability(phi_1.4,alpha_1.4)

#Combine both scenarios into matrix
phi_V<-cbind(phi_1, phi_1.1) 
response_rate<-cbind(res_rate1, res_rate1.1) 
response_rate.p<-cbind(res_rate1.p, res_rate1.1.p) 

N=100 # total number of patients
Nreps=3 # number of trial replications

#Example of how could loop for scenarios 1.1 & 1.2
task_ID=2 #No. of scenarios to test in this script

for (task_id in 1:task_ID) {
res_rate_mat<-matrix(response_rate[,task_id], byrow = T,
                     nrow= length(pattern), ncol= no_treatment)

res_rate_mat_prior<-matrix(response_rate.p[,task_id], byrow = T,
                     nrow= length(pattern), ncol= no_treatment)

scenario_out<-simulation(N=N, phi_v=phi_V[,task_id], 
                            pattern=patternV, 
                            res_probability_prior=res_rate_mat_prior,
                            res_probability_all=res_rate_mat,
                            prob_pattern=prob_pattern, R=Nreps)
filename<-paste0("scenario_A_", task_id, ".RData")
save.image(file=filename) # task_id
}


#Example of how could loop scenario 1.1 for different sample sizes
samplesize_vec<-seq(500, 3000, 500)

for (N in samplesize_vec) {
  res_rate_mat<-matrix(res_rate1, byrow = T,
                       nrow= length(pattern), ncol= no_treatment)
  
  res_rate_mat_prior<-matrix(res_rate1.p, byrow = T,
                             nrow= length(pattern), ncol= no_treatment)
  
  scenario_out<-simulation(N=N, phi_v=phi_1, 
                           pattern=patternV, 
                           res_probability_prior=res_rate_mat_prior,
                           res_probability_all=res_rate_mat,
                           prob_pattern=prob_pattern, R=Nreps)
  filename<-paste0("scenario_A_", N, ".RData")
  save.image(file=filename) # task_id
}



