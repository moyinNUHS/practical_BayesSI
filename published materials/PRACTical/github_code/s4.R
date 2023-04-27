source("1 PRaCTical_functions.R")
no_treatment=10 

# treatment effect parameters of scenario 1
alpha_1<-find_phi(0.2, alpha=0)
phi_1<-find_phi(seq(0.1, 0.3, length.out = no_treatment), alpha=alpha_1)
res_rate1<-res_probability(phi_1,alpha_1)

generate_p<-function(nt,np){
t<-t(combn(1:10,nt))
x<-t[sample(1:dim(t)[1], np),]
split(x, row(x))
}
set.seed(431)
l1<-generate_p(3,50)
l2<-generate_p(5,50)
l3<-generate_p(7,50)
l4<-generate_p(6,40)
l5<-split(t(combn(1:10,9)), row(t(combn(1:10,9))))
pattern<-c(l1,l2,l3,l4,l5)
names(pattern)<-NULL

res_rate_mat<-matrix(res_rate1, byrow = T,
                     nrow= length(pattern), ncol= no_treatment)


Nobs=2000; Nreps=1000
set.seed(103)
scenario_out<-simulation(N=2000, phi_v=phi_1, 
                          pattern=pattern, 
                          res_probability_all=res_rate_mat,  
                          prob_pattern=rep(1/200, 200), R=Nreps)
filename<-paste0("scenario_D_method.RData")
save.image(file=filename)   

