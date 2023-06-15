# ---------------------------------------- #
# method D: fit duplicate data in one step # 
# ---------------------------------------- #

fit_robustSE_D<-function(alldata=Alldata){
  #no_com=no_comparison; no_p=no_pattern; size_p=size_pattern
  no_com<-no_comparison
  no_t<-no_treatment
  no_p<-no_pattern
  size_p<-size_pattern
  dup_data<-sapply(1:no_p,function(i){cbind(rep(alldata[1,][[i]], each=no_com[i]),
                                            rep(alldata[2,][[i]], each=no_com[i]),
                                            alldata[3,][[i]] , rep(i, no_com[i]) ) } )
  dup_data<-as.data.frame(do.call(rbind, dup_data),make.names=F )
  colnames(dup_data)<-c("y", "treatment", "pairwise", "pattern")
  dup_data$treatment<-factor(dup_data$treatment, levels = 1:no_treatment)
  
  # create ID of patients
  id_v<-cumsum(size_p)
  ID<-sapply(1:(no_p-1),function(i){
    ids<-(id_v[i]+1):id_v[i+1]
    rep(ids,each=no_com[1+i])
  })
  ID<-c(rep(1:id_v[1], each=no_com[1]), unlist(ID))
  
  dup_data$id<-ID
  
  
  pairwisev<-sapply(1:no_t,function(i)paste(i, 1:no_t, sep="_"))
  v1=t(pairwisev)[upper.tri(pairwisev)]
  v2=pairwisev[upper.tri(t(pairwisev))]
  
  val<-dup_data$pairwise#val<-paste(dup_data$treatment, dup_data$pairwise, sep="_")
  for(z in 1:(no_t*(no_t-1)/2)){
    test<-which(val==v2[z])
    if(length(test)==0){}else{val[test]<-v1[z]}
  }
  dup_data$id_comparison<-factor(val)
  
  #Still part of method D
  # ------------------------- **note 1** ------------------------------------------ #
  # some R version (eg R version 3.6.0 on linux) would convert the class of y to factor, 
  # if so, use the following line instead
  #fit_glm<-myTryCatch( glm(as.numeric(levels(y))[y]~treatment+id_comparison,family="binomial",data=dup_data) )
  
  fit_glm<-myTryCatch( glm(as.numeric(y)~treatment+id_comparison,family="binomial",data=dup_data) )
  
  if(is.null(fit_glm$error) ) #if there is no error, model is fitted 
  { 
    fit_glm<-fit_glm[[1]]
    # Calculate robust standard errors #
    cov.m1 <- cluster.vcov(fit_glm, dup_data$id)[2:no_t, 2:no_t]#vcovHC(fit_glm, type = "HC0")[2:no_t, 2:no_t]
    testcov<-myTryCatch( sqrt(diag(cov.m1)) )
    std.err <- if(is.null(testcov$warning) & is.null(testcov$error) ){testcov$value}else{rep(NaN,no_t-1)} 
    
    q.val <- qnorm(0.975)
    r.est <- cbind( Estimate = coef(fit_glm)[2:no_t], 
                    model_var=std.err^2, #"Robust SE" = std.err, 
                    z = (coef(fit_glm)[2:no_t]/std.err),
                    #"Pr(>|z|) "= 2 * pnorm(abs(coef(fit_glm)[2:no_t]/std.err), lower.tail = FALSE),
                    LL = coef(fit_glm)[2:no_t] - q.val  * std.err,
                    UL = coef(fit_glm)[2:no_t] + q.val  * std.err)
    
    r.est[which(abs(r.est[,1])>12),]<-NA # unconverged parameter is set to NA
  } else{ # there is error in model fit
    r.est<-matrix(rep(NA,(no_t-1)*5),nrow = no_t-1, ncol = 5 )
    r.est[,5]<-fit_glm$error[1]$message
  }  
  
  # for each subgroup, prepare the coefficients to identify rankings
  prep.coeff<-function(i){
    sub_data<-alldata[,i]
    t_label<-sort(unique(sub_data$treatment_label)) 
    
    if(is.null(fit_glm$error) ) # if there is no error in model fit (there could be warning)
    { 
      
      fit.coeff<-c(0, r.est[,1])      
      est.contrasts<-rep(NA, no_treatment)
      est.contrasts[t_label]<-fit.coeff[t_label]
      
    }else{ est.contrasts<-rep(NA, no_treatment) }
    
    return(find.rankings(t_labelv=t_label, treat.coeff=est.contrasts) )
    
  }
  
  rank.v<-sapply(1:no_p, prep.coeff)
  colnames(rank.v)<-sapply(1:no_pattern, function(i)paste("pattern", i))
  row.names(rank.v)<-c("suggested treatment", "model.not.fit")
  
  
  return(list(contrast.est=r.est, ranking=rank.v) )
  
}  
