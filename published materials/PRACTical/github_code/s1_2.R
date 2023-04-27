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
# scenario 1.2
alpha_1.2<-find_phi(seq(0.05, 0.6, length.out = 8), alpha=0)
alpha_1.2[3]<-find_phi(0.2,alpha=0)
phi_1.2<-find_phi(seq(0.1, 0.3, length.out = no_treatment), alpha=alpha_1.2[3])
#res_rate1.2<-res_probability(phi_1.2,alpha_1.2[3])
res_rate_mat<-t(sapply(alpha_1.2, function(i)res_probability(phi_1.2,i)))
res_rate_mat<-res_rate_mat[c(3,1,2,4:8),]

Nobs=1000; Nreps=50000
set.seed(103)
scenario_out<-simulation(N=1000, phi_v=phi_1.2, 
                         pattern=patternV, 
                         res_probability_all=res_rate_mat, 
                         prob_pattern= c(0.2, 0.2, rep(0.1, 6)), R=50000)
filename<-paste0("scenario_A_3.RData")
save.image(file=filename) # task_id=1
warnings()