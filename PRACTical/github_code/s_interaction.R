source("1 PRaCTical_functions.R")
no_treatment=10 

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


set.seed(230)
rand_id<-lapply(pattern3, function(x) sample(x, ceiling(length(x)/2)))


# scenario 5.1 treatment effect parameters
alpha_1<-find_phi(0.2, alpha=0)
phi_j<-find_phi(seq(0.1, 0.3, length.out = no_treatment), alpha=alpha_1)

# ------------------------ #
# quantitative interaction #
# ------------------------ #
new_phi<-function(k){
  change_phi<-phi_j
  change_phi[rand_id[[k]]]<-0
  return(change_phi)
}

all_phi<-t(sapply(1:20, new_phi))

res_rate_mat_5.1<-t(sapply(1:20,function(k)res_probability(all_phi[k,],alpha_1) ))



# scenario 5.2 treatment effect parameters
alpha_k<-find_phi(seq(0.05, 0.6, length.out = 20), alpha=0)
alpha_k[6]<-alpha_1

res_rate_mat_5.2<-t(sapply(1:20,function(k)res_probability(all_phi[k,],alpha_k[k]) ))


# scenario 6.2 treatment effect parameters


# ----------------------- #
# qualitative interaction #
# ----------------------- #

new_phi2<-function(k){
  change_phi<-phi_j
  change_phi[rand_id[[k]]]<- -change_phi[rand_id[[k]]]
  return(change_phi)
}

all_phi2<-t(sapply(1:20, new_phi2))


res_rate_mat_6.1<-t(sapply(1:20,function(k)res_probability(all_phi2[k,],alpha_1) ))
res_rate_mat_6.2<-t(sapply(1:20,function(k)res_probability(all_phi2[k,],alpha_k[k]) ))



res_mat<-list(res_rate_mat_5.1, res_rate_mat_5.2, res_rate_mat_6.1, res_rate_mat_6.2)

task_id_string <- Sys.getenv("SLURM_ARRAY_TASK_ID")
task_id <- as.numeric(task_id_string) # 
Nobs=1000; Nreps=50000
pattern=pattern3
set.seed(103)
scenario_out<-simulation(N=1000, phi_v=phi_j, 
                         pattern=pattern3, 
                         res_probability_all=res_mat[[task_id]], 
                         prob_pattern= rep(1/20, 20), R=50000)
filename<-paste0("scenario_interaction_", task_id, ".RData")
save.image(file=filename) # task_id=1
warnings()