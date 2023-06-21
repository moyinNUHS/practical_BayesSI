# 1/7/2021 change the title to (%)
rm(list=ls())
# -------------------- #
# response probability #
# -------------------- #
res_probability<-function(phi,alpha=-1.36){
  exp(alpha+ phi)/(1+ exp(alpha+ phi) )
}

find_phi<-function(p, alpha=-1.36){log(p/(1-p)) - alpha } # logit for finding phi

alpha_1<-find_phi(0.2, alpha=0) # value of intercept doesn't affect contrast
phi_1<-find_phi(seq(0.1, 0.3, length.out = 10), alpha=alpha_1)

true_contrast<-phi_1-phi_1[1]
true_contrast<-true_contrast[-1]


labv=c("interaction_1", "interaction_2", "interaction_3", "interaction_4")
MethodC<-MethodD<-estimand2<-estimand2_mcse<-mortality_sign<-list()
for(i in 1:length(labv)){
  lab=labv[i]
  
  #scenario=paste0("/Users/kimlee/OneDrive/Projects/9 PRaCTical/simulation/hpc/scenario_",lab,".RData")
  scenario=paste0("/Users/kimlee/Desktop/PRaCTical/simulation/hpc/scenario_",lab,".RData")
  
  load(scenario)
  MethodC[[labv[i]]]<-scenario_out$method_C_property[,1:7]
  MethodD[[labv[i]]]<-scenario_out$method_D_property[,1:7]
  estimand2_mcse[[labv[i]]]<-scenario_out$estimand2_MCSE
  estimand2[[labv[i]]]<-scenario_out$estimand2 
  mortality_sign[[i]]<-apply(scenario_out$ex_performance_out$mortality_gain,1,function(x)length(which(x>0)))
}


bias_all_C<-sapply(1:length(labv),function(i)MethodC[[labv[i]]][,1]) #colnames(bias_all_C)<-labv
coverage_all_C<-sapply(1:length(labv),function(i)MethodC[[labv[i]]][,3])
mse_all_C<-sapply(1:length(labv),function(i)MethodC[[labv[i]]][,4])
MCSE_bias_all_C<-sapply(1:length(labv),function(i)MethodC[[labv[i]]][,5])  
MCSE_coverage_all_C<-sapply(1:length(labv),function(i)MethodC[[labv[i]]][,6])
MCSE_mse_all_C<-sapply(1:length(labv),function(i)MethodC[[labv[i]]][,7])

bias_all_D<-sapply(1:length(labv),function(i)MethodD[[labv[i]]][,1])
coverage_all_D<-sapply(1:length(labv),function(i)MethodD[[labv[i]]][,3])
mse_all_D<-sapply(1:length(labv),function(i)MethodD[[labv[i]]][,4])
MCSE_bias_all_D<-sapply(1:length(labv),function(i)MethodD[[labv[i]]][,5])
MCSE_coverage_all_D<-sapply(1:length(labv),function(i)MethodD[[labv[i]]][,6])
MCSE_mse_all_D<-sapply(1:length(labv),function(i)MethodD[[labv[i]]][,7])


mortality_gain<-sapply(1:length(labv),function(i)estimand2[[labv[i]]][,1])
better<-sapply(1:length(labv),function(i)estimand2[[labv[i]]][,2])
best<-sapply(1:length(labv),function(i)estimand2[[labv[i]]][,3])
nearbest5<-sapply(1:length(labv),function(i)estimand2[[labv[i]]][,4])
nearbest10<-sapply(1:length(labv),function(i)estimand2[[labv[i]]][,5])

MCSE_mortality_gain<-sapply(1:length(labv),function(i)estimand2_mcse[[labv[i]]][,1])
MCSE_better<-sapply(1:length(labv),function(i)estimand2_mcse[[labv[i]]][,2])
MCSE_best<-sapply(1:length(labv),function(i)estimand2_mcse[[labv[i]]][,3])
MCSE_nearbest5<-sapply(1:length(labv),function(i)estimand2_mcse[[labv[i]]][,4])
MCSE_nearbest10<-sapply(1:length(labv),function(i)estimand2_mcse[[labv[i]]][,5])

rel_bias_all_C<-apply(bias_all_C, 2,function(x){x/true_contrast *100} )
rel_bias_all_D<-apply(bias_all_D, 2,function(x){x/true_contrast *100} )

apply(rel_bias_all_C, 2, function(x)c(mean(x), max(abs(x))) )
apply(rel_bias_all_D, 2, function(x)c(mean(x), max(abs(x))) )

colnames(mortality_gain)<- colnames(MCSE_mortality_gain) <-c("S 5.1", "S 5.2", "S 6.1", "S 6.2")
load("/Users/kimlee/Desktop/PRaCTical/simulation/hpc/scenario_interaction_pattern_oracle.RData") #6.2
oracle_mortality_gain
# -0.05468607 -0.08078722 -0.07259350 -0.09164523 


# ------------------------------------------------------------------------------------- #
# estimand 2: plot all scenario in one plot, different shape represent different method #
# ------------------------------------------------------------------------------------- #
dev.new()
par(mar=c(0, 1, 1.5, 10), mfrow=c(5,1), oma = c(3, 3, 1, 1), xpd=F)
colv<-c("black",  "red", "darkgreen", "deepskyblue2", "blueviolet", "brown", "darkorange")
vall<-cbind(seq(1,4,by=0.5),
            seq(7,10, by=0.5),
            seq(13,16,by=0.5),
            seq(19,22, by=0.5))



# for mortality plot that has a different y-axis range
Val_C=-mortality_gain*100
plot(1:24, rep(NA,24), xaxt = "n", yaxt = "n", ylim = c(0 ,6.1), 
     ylab="",
     xlab="")
sapply((1:6), function(j){
  points(vall[j,], Val_C[j,], pch=7-j, cex=1.25, col=colv  )
})

title(expression(paste("Mortality gain (%)") ), cex.main=1.5)
#axis(side=2, tick=F, at=3, labels=expression(paste("(",10^-2, ")") ), padj=-1, cex.axis=1.5)
axis(side = 2, at = c(0,2,4,6), labels=c(0,2,4,6), las=2, cex.axis=1.5) #, col.axis="grey40")
abline(h=0:6, lty=3, col="grey80")
legend("topright",legend=c("A ","B1 ", "B2 ", "B3 ", "C ", "D "), title="Methods",pch=7-1:6, ncol=2,
       xpd = T, horiz = F, inset = c(-0.125, 0), cex=1.4)

# plot the other measures #
plot_alls2<-function(Val_C){
  #Val_C=better  
  plot(1:24, rep(NA,24), xaxt = "n", yaxt = "n", ylim = c(20 ,100  ), ylab="" ,xlab="")
  sapply((1:6), function(j){
    points(vall[j,], Val_C[j,], pch=7-j, cex=1.25, col=colv  )
  })
  abline(h=seq(20,100,by=10), lty=3, col="grey80")
  axis(side = 2, at = seq(20,100,by=20), labels=seq(20,100,by=20), las=2, cex.axis=1.5)  
  
}

plot_alls2(better*100)
all_mean<-round(apply(better, 2, mean),2)
title(expression(paste("Better treatment probability (%)") ), line = 0.5, cex.main=1.5)


plot_alls2(best*100)
title(expression(paste("Best treatment probability (%)") ), line = 0.5, cex.main=1.5)

plot_alls2(nearbest5*100)
title(expression(paste("Near best treatment probability (%), ", kappa, "=5%") ), line = 0.5, cex.main=1.5)


Val_C=nearbest10*100 ;  
plot(1:24, rep(NA,24), xaxt = "n", yaxt = "n", ylim = c(20, 100), ylab="" ,xlab="")
sapply((1:6), function(j){
  points(vall[j,], Val_C[j,], pch=7-j, cex=1.25, col=colv  )
})
abline(h=seq(20,100,by=10), lty=3, col="grey80")
axis(side = 2, at = seq(20,100,by=20), labels=seq(20,100,by=20), las=2, cex.axis=1.5)  

title(expression(paste("Near best treatment probability (%), ", kappa, "=10%") ), line = 0.5, cex.main=1.5)
xxx=vall[3,]+0.5
sapply(1:4,function(i)
{axis(side=1, at=xxx[i] ,labels = c("S5.1", "S5.2", "S6.1", "S6.2")[i], col.axis=colv[i], cex.axis=1.5)} )


dev.print(pdf, '/Users/kimlee/Desktop/PRaCTical/plot/estimand2_intv_01072021.pdf')
