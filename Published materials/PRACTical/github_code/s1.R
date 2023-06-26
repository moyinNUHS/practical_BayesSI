source("1 PRaCTical_functions.R")
no_treatment=10 

# treatment patterns
pattern1<-c(2,3,5,8,10)
pattern2<-1:7
pattern3<-c(1,2,4,9,10)
pattern4<-c(1,2,3,5,6,8,10)
pattern5<-c(1,2,3,4,6,7)
pattern6<-2:10
pattern7<-1:10
pattern8<-3:10
patternV<-list(pattern1, pattern2, pattern3, pattern4, pattern5, pattern6, pattern7, pattern8)

pattern<-patternV
# treatment effect parameters
# scenario 1
alpha_1<-find_phi(0.2, alpha=0)
phi_1<-find_phi(seq(0.1, 0.3, length.out = no_treatment), alpha=alpha_1)
res_rate1<-res_probability(phi_1,alpha_1)

# scenario 1.1
alpha_1.1<-find_phi(0.2, alpha=0)
phi_1.1<-rep(0,10)
res_rate1.1<-res_probability(phi_1.1,alpha_1.1)

phi_V<-cbind(phi_1, phi_1.1) 
response_rate<-cbind(res_rate1, res_rate1.1) 

task_id_string <- Sys.getenv("SLURM_ARRAY_TASK_ID")
task_id <- as.numeric(task_id_string) # task_id=1


res_rate_mat<-matrix(response_rate[,task_id], byrow = T,
                     nrow= length(pattern), ncol= no_treatment)
# row= pattern, column=treatment
Nobs=1000; Nreps=50000
set.seed(103)
scenario_out<-simulation(N=1000, phi_v=phi_V[,task_id], 
                         pattern=patternV, 
                         res_probability_all=res_rate_mat, 
                         prob_pattern= c(0.2, 0.2, rep(0.1, 6)), R=Nreps)
filename<-paste0("scenario_A_", task_id, ".RData")
save.image(file=filename) # task_id=1
warnings()
