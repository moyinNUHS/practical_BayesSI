# ------------------------------------------------------------------------- #
# method C extension non-informative prior: fit one step model to all data  # 
# ------------------------------------------------------------------------- #

fit_onestage_C_NI<-function(alldata=Alldata){
  no_p<-no_pattern
  
  loc_NI<-0
  scale_NI<-5
  prior <- student_t(df = 7, loc_NI, scale_NI)
  prior_int <- student_t(df = 7, loc_NI, scale_NI)
  
  nma_data<-data.frame(y=unlist(alldata[1,]),
                       treatment=factor(unlist(alldata[2,]), levels = sort(unique(unlist(alldata[2,])))),
                       subgroup=factor(unlist(alldata[4,])))#patient_subgroup)))
  
  my.glm<-myTryCatch(stan_glm(y~treatment + subgroup, data = nma_data, prior = prior,
                              prior_intercept = prior_int, family = binomial(link = "logit"), 
                              cores = 2) )
  #use of default priors in stan_glm are non-informative
  q.val<-qnorm(0.975)
  
  if(is.null(my.glm$error) ) #if do not have an error, model is fitted
  { 
    my.glm<-my.glm[[1]]
    mof<-posterior_interval(my.glm, prob = 0.95)
    std.err<-sqrt(diag(vcov(my.glm))[2:no_treatment])
    out<-cbind(Estimate=my.glm$coefficients[2:no_treatment], #get_estimates(my.glm, centrality = "mean")[2:no_treatment, 2],
               model_var=std.err^2,
               z=my.glm$coefficients[2:no_treatment]/std.err, #get_estimates(my.glm, centrality = "mean")[2:no_treatment, 2]/std.err,
               LL=mof[2:no_treatment, 1],
               UL=mof[2:no_treatment, 2])
    out[which(abs(out[,1])>12),]<-NA #parameter not converged is set to NA 
    
  }else
  { # if there is error, do not fit model
    out<-matrix(rep(NA,(no_treatment-1)*5),nrow = no_treatment-1, ncol = 5 )
    out[1,5]<-my.glm$error[1]$message
    
  } 
  
  
  # for each subgroup, prepare the coefficients to identify rankings
  prep.coeff<-function(i){
    sub_data<-alldata[,i]
    t_label<-sort(unique(sub_data$treatment_label)) 
    
    if(is.null(my.glm$error) ) # if there is no error in model fit (there could be warning)
    { 
      
      fit.coeff<-c(0, out[,1])      
      est.contrasts<-rep(NA, no_treatment)
      est.contrasts[t_label]<-fit.coeff[t_label]
      
    }else{ est.contrasts<-rep(NA, no_treatment) }
    
    return(find.rankings(t_labelv=t_label, treat.coeff=est.contrasts) )
    
  }
  
  rank.v<-sapply(1:no_p, prep.coeff)
  
  colnames(rank.v)<-sapply(1:no_pattern, function(i)paste("pattern", i))
  row.names(rank.v)<-c("suggested treatment", "model.not.fit")
  
  return(list(contrast.est=out, ranking=rank.v) )
}
