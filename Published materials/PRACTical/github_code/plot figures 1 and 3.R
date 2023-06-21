# 1/7/2021 change the title to (%)
# created on 22/2/2021 to enlarge the font size and change the location of legend
# update also the MCSE of relative bias
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


labv=c("A_1", "A_2", "A_3", "B", "C_1", "C_2", "C_3", "D_all" )  
MethodC<-MethodD<-estimand2<-estimand2_mcse<-list()
for(i in 1:length(labv)){
  lab=labv[i]
  
  #scenario=paste0("/Users/kimlee/OneDrive/Projects/9 PRaCTical/simulation/hpc/scenario_",lab,".RData")
  scenario=paste0("/Users/kimlee/Desktop/PRaCTical/simulation/hpc/scenario_",lab,".RData")
  
  load(scenario)
  MethodC[[labv[i]]]<-scenario_out$method_C_property[,1:7]
  MethodD[[labv[i]]]<-scenario_out$method_D_property[,1:7]
  estimand2_mcse[[labv[i]]]<-scenario_out$estimand2_MCSE
  estimand2[[labv[i]]]<-scenario_out$estimand2 
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

rel_bias_all_C<-apply(bias_all_C, 2,function(x){x/true_contrast *100} )[,-2]
rel_bias_all_D<-apply(bias_all_D, 2,function(x){x/true_contrast *100} )[,-2]

apply(rel_bias_all_C, 2, function(x)c(mean(x), max(abs(x))) )
apply(rel_bias_all_D, 2, function(x)c(mean(x), max(abs(x))) )



# ---------------------------------------------  #
#   estimand 1: plot all scenario in one plot    #
#                                                #
# ---------------------------------------------- #
dev.new()
par(mar=c(0, 0.5, 1.5, 9), mfrow=c(3,1), oma = c(3, 5, 1, 1), xpd=F)

colv<-c("black",   "red", "darkgreen", "deepskyblue2", "blueviolet", "brown", "darkorange")
allv<-c(as.vector(rel_bias_all_C), as.vector(rel_bias_all_D))
xval<-c(0,1:24)
plot(xval, rep(NA,length(xval)), xaxt = "n", yaxt = "n", 
     ylim = c(min(allv)-3, max(allv)), ylab="" ,xlab="")

# for bias plot exclude situation 1.1 since that has zero treatment contrast #
add_points_b<-function(i){ # i=1
  Location<-rbind(seq(1,24,by=3), seq(1,24,by=3)+1)#rbind(seq(1,15,by=2), seq(1,15,by=2)+0.5)
  Location<-Location[,-2]
  set.seed(10)
  points(jitter( rep(Location[1,i], 9 ), 1, amount = 0.5 ), rel_bias_all_C[,i], pch=2, col=colv[i])
  set.seed(10)
  points(jitter( rep(Location[2,i], 9 ), 1, amount = 0.5 ), rel_bias_all_D[,i], pch=1, col=colv[i])
} #add_points(1])

sapply(1:7, add_points_b)

abline(h=c(-10, 0, 10), lty=3, col="grey60")
axis(side = 2, at = c(-10,0,10), labels=c(-10,0,10), las=2, cex.axis=1.5)
title("Relative bias of treatment contrasts (%)", cex.main=1.5)
text(x=19, y=-14, labels = "MCSE is < 1.19 for all scenarios except S4 (<5.42)" , cex=1.5)


legend("topright", inset=c(-0.12,0),legend=c("Method C", "Method D"), pch=c(2,1), #ncol=6,
       xpd = T, horiz = F,  cex=1.5)


# for MSE and coverage plot  #
add_points<-function(i, all_C, all_D){ # i=1; all_C=mse_all_C*100; all_D=mse_all_D*100
  Location<-rbind(seq(1,24,by=3), seq(1,24,by=3)+1)#rbind(seq(1,15,by=2), seq(1,15,by=2)+0.5)
  locv<-Location[,i]
  set.seed(10)
  points(jitter( rep(Location[1,i], 9 ), 1, amount = 0.5 ), all_C[,i], pch=2, col=colv[i])
  set.seed(10)
  points(jitter( rep(Location[2,i], 9 ), 1, amount = 0.5 ), all_D[,i], pch=1, col=colv[i])
} #add_points(1])

# MSE #
colv<-c("black", "blue", "red", "darkgreen", "deepskyblue2", "blueviolet", "brown", "darkorange")


allv<-c(as.vector(mse_all_C*100), as.vector(mse_all_D*100))

plot(xval, rep(NA,length(xval)), xaxt = "n", yaxt = "n", 
     ylim = c(min(allv)-3, max(allv)+0.02), ylab="" ,xlab="")

MSE<-function(j)add_points(j, mse_all_C*100, mse_all_D*100)
sapply(1:8, MSE)
abline(h=c(10,20,30,40 ), lty=3, col="grey60")
axis(side = 2, at = c(10,20,30,40 ), labels=c(10,20,30,40 ), las=2, cex.axis=1.5)
title("Mean squared error (x 100)", cex.main=1.5)
largest_MCSE<-max(c(as.vector(MCSE_mse_all_C),as.vector(MCSE_mse_all_D)) )
text(x=20.5, y=6, labels = "MCSE is < 0.82 for all scenarios" , cex=1.5)



# coverage probability
allv<-c(as.vector(coverage_all_C*100), as.vector(coverage_all_D*100))

plot(xval, rep(NA,length(xval)), xaxt = "n", yaxt = "n", 
     ylim = c(min(allv)-1, max(allv)+0.6), ylab="" ,xlab="")

coverage<-function(j)add_points(j, coverage_all_C*100, coverage_all_D*100)
sapply(1:8, coverage)
abline(h=c(93, 95,97 ), lty=3, col="grey60")
axis(side = 2, at = c(93, 95,97 ), labels=c(93, 95,97 ), las=2, cex.axis=1.5)
title("Coverage probability (x 100%)", cex.main=1.5)
max( c(as.vector(MCSE_coverage_all_C ), as.vector(MCSE_coverage_all_D )) )
text(x=20.5, y=91.5, labels = "MCSE is < 0.84 for all scenarios" , cex=1.5)



s_lab<-c("S1", "S1.1", "S1.2", "S2", "S3", "S3.1", "S3.2", "S4")
#text(x=seq(1,24,by=3)+1/2, y=0.905*100, s_lab,      cex=1.2, col = colv )
vall<-seq(1,24,by=3)+0.5
sapply(1:8,function(i){axis(side = 1, at = vall[i], labels=s_lab[i], col.axis = colv[i],  
                            cex.axis=1.5)} )

dev.print(pdf, '/Users/kimlee/Desktop/PRaCTical/plot/estimand1.pdf')


# ------------------------------------------------------------------------------------- #
# estimand 2: plot all scenario in one plot, different shape represent different method #
# ------------------------------------------------------------------------------------- #
colv<-c("black",  "red", "darkgreen", "deepskyblue2", "blueviolet", "brown", "darkorange")
s_lab<-c("S1",  "S1.2", "S2", "S3", "S3.1", "S3.2", "S4")
vall<-matrix(1:42,ncol=7)
dev.new()
par(mar=c(0, 1, 1.5, 10), mfrow=c(5,1), oma = c(3, 3, 1, 1), xpd=F)

Val_C=-mortality_gain*100
plot(1:42, rep(NA,42), xaxt = "n", yaxt = "n", ylim = c(0 , 10  ), ylab="" ,xlab="")
sapply((1:6), function(j){
  points(vall[j,], Val_C[j,-2], pch=7-j, cex=1.25, col=colv  )
})

title(expression(paste("Mortality gain (%)") ), cex.main=1.5)
axis(side = 2, at = seq(0,10,2), labels=seq(0,10,2), las=2, cex.axis=1.5) #, col.axis="grey40")
abline(h=seq(0,10,1), lty=3, col="grey80")

legend("topright",legend=c("A ","B1 ", "B2 ", "B3 ", "C ", "D "), title="Methods",pch=7-1:6, ncol=2,
       xpd = T, horiz = F, inset = c(-0.125, 0), cex=1.4)

plot_alls2<-function(Val_C){
  #Val_C=better  
  plot(1:42, rep(NA,42), xaxt = "n", yaxt = "n", ylim = c(50 , 100  ), ylab="" ,xlab="")
  sapply((1:6), function(j){
    points(vall[j,], Val_C[j,-2], pch=7-j, cex=1.25, col=colv  )
  })
  abline(h=seq(50, 100, 10), lty=3, col="grey80")
  axis(side = 2, at = seq(60, 100, 20), labels=seq(60, 100, 20), las=2, cex.axis=1.5) #, col.axis="grey40")
  
}


plot_alls2(better*100)
all_mean<-round(apply(better, 2, mean),2)
title(expression(paste("Better treatment probability (%)") ), line = 0.5, cex.main=1.5)


plot_alls2(best*100)
all_mean<-round(apply(best, 2, mean),2)
title(expression(paste("Best treatment probability (%)") ), line = 0.5, cex.main=1.5)

plot_alls2(nearbest5*100)
all_mean<-round(apply(nearbest5, 2, mean),2)
title(expression(paste("Near best treatment probability (%), ", kappa, "=5%") ), line = 0.5, cex.main=1.5)

plot_alls2(nearbest10*100)
all_mean<-round(apply(nearbest10, 2, mean),2)
title(expression(paste("Near best treatment probability (%), ", kappa, "=10%") ), line = 0.5, cex.main=1.5)


s_lab<-c("S1", "S1.2", "S2", "S3", "S3.1", "S3.2", "S4")
xl=c(vall[4,], 46)
sapply(1:8,function(i){axis(side = 1, at = xl[i], labels=s_lab[i], col.axis = colv[i],  
                            cex.axis=1.5)} )




dev.print(pdf, '/Users/kimlee/Desktop/PRaCTical/plot/estimand2_01072021.pdf')
