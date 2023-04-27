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

set.seed(8)
draw_pattern<-sample(1:8, size=3) # 8 4 2

pattern<-sapply(draw_pattern, function(x)patternV[[x]])
# treatment effect parameters
# scenario 1
alpha_1<-find_phi(0.2, alpha=0)
phi_1<-find_phi(seq(0.1, 0.3, length.out = no_treatment), alpha=alpha_1)
res_rate1<-res_probability(phi_1,alpha_1)

res_rate_mat<-matrix(res_rate1, byrow = T,
                     nrow= length(pattern), ncol= no_treatment)

#N=1000; phi_v=phi_1; pattern=pattern3; response_prob_V=res_rate1; prob_pattern=rep(0.1, length(pattern3) ); R=5
Nobs=1000; Nreps=50000
set.seed(103)
scenario_out<-simulation(N=1000, phi_v=phi_1, 
                         pattern=pattern, 
                         res_probability_all=res_rate_mat,  
                         prob_pattern=rep(1/3, 3), R=Nreps)

save.image(file="scenario_B.RData")  
warnings()
