source("1 PRaCTical_functions.R")
no_treatment=10 

# treatment effect parameters of scenario 1
alpha_1<-find_phi(0.2, alpha=0)
phi_1<-find_phi(seq(0.1, 0.3, length.out = no_treatment), alpha=alpha_1)
res_rate1<-res_probability(phi_1,alpha_1)


#scenario 3: 5 patterns each with 3 randomly chosen treatments, 5 with 5, 5 with 7, 5 with 9
#scenario 3.1:each with 3 randomly chosen treatments
#scenario 3.2:each with 7 randomly chosen treatments

generate_p<-function(nt,np){
  t<-t(combn(1:10,nt))
  x<-t[sample(1:dim(t)[1], np),]
  split(x, row(x))
}

set.seed(431)
l1<-generate_p(3,5)
l2<-generate_p(5,5)
l3<-generate_p(7,5)
l4<-generate_p(9,5)
pattern3<-c(l1,l2,l3,l4); names(pattern3)<-NULL

set.seed(4311)
pattern3_1<-generate_p(3,20)

set.seed(311)
pattern3_2<-generate_p(7,20)



PATTERN<-list(pattern3, pattern3_1, pattern3_2)
task_id_string <- Sys.getenv("SLURM_ARRAY_TASK_ID")
task_id <- as.numeric(task_id_string) # task_id=1 
pattern<-PATTERN[[task_id]]
res_rate_mat<-matrix(res_rate1, byrow = T,
                     nrow= length(pattern), ncol= no_treatment)
# row= pattern, column=treatment

Nobs=1000; Nreps=50000
set.seed(103)
scenario_out<-simulation(N=1000, phi_v=phi_1, 
                         pattern=PATTERN[[task_id]], 
                         res_probability_all=res_rate_mat,  
                         prob_pattern=rep(1/20, 20), R=50000)


filename<-paste0("scenario_C_", task_id, ".RData")
save.image(file=filename) # task_id=1
warnings()