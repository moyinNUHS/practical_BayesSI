fit_onestage_C_hier_prior<-function(alldata_prior=Alldata_prior, alldata=Alldata, Scale){
  no_p<-no_pattern
  
  nma_data_prior<-data.frame(y=unlist(alldata_prior[1,]),
                             treatment=factor(unlist(alldata_prior[2,]), levels = sort(unique(unlist(alldata_prior[2,])))),
                             subgroup=factor(unlist(alldata_prior[4,]))#,
                            #site=factor(unlist(alldata_prior[5,]))
                             )
  my.glm_prior<-glmer(y~treatment + (1 | subgroup),family="binomial",data=nma_data_prior) 
 # my.glm_prior<-glmer(y~treatment + (1 | site:subgroup),family="binomial",data=nma_data_prior) 
  my.glm_prior_coeff<-fixef(my.glm_prior)
  
  prior <- normal(location = my.glm_prior_coeff[2:no_treatment], scale = rep(Scale, (no_treatment-1)), autoscale=TRUE)
  prior_int <- normal(location = my.glm_prior_coeff[1], scale = Scale, autoscale=TRUE)
  
  nma_data<-data.frame(y=unlist(alldata[1,]),
                       treatment=factor(unlist(alldata[2,]), levels = sort(unique(unlist(alldata[2,])))),
                       subgroup=factor(unlist(alldata[4,]))#,
                       #site=factor(unlist(alldata[5,]))
                       )
  
  my.glm<-myTryCatch(stan_glmer(y~treatment + (1 | subgroup), data = nma_data, prior = prior,
                              prior_intercept = prior_int, family = binomial(link = "logit"), 
                              cores = 1, refresh=0) )
 ### my.glm<-myTryCatch(stan_glmer(y~treatment + (1 | site:subgroup), data = nma_data, prior = prior,
   #                           prior_intercept = prior_int, family = binomial(link = "logit"),
   #                           cores = 1, refresh=0) )
  
  if(is.null(my.glm$error) ) #if do not have an error, model is fitted
  { 
    my.glmm<-my.glm[[1]]
   # Treat.best<-which.min(c(0, my.glmm$coefficients[2:no_treatment]))
   # if (Treat.best==1){
      mof<-posterior_interval(my.glmm, prob = 0.95)
      std.err<-my.glmm$ses[2:no_treatment]
      out<-cbind(Estimate=my.glmm$coefficients[2:no_treatment],
               model_var=std.err^2,
               z=my.glmm$coefficients[2:no_treatment]/std.err,
               LL.1=mof[2:no_treatment, 1],
               UL.1=mof[2:no_treatment, 2])
      out[which(abs(out[,1])>12),]<-NA #parameter not converged is set to NA 
    #} else {
    #  my.glmm<-stan_glmer(y~relevel(treatment, ref = Treat.best) + (1 | subgroup), data = nma_data, prior = prior,
    #                     prior_intercept = prior_int, family = binomial(link = "logit"), 
    #                     cores = 1, refresh=0) 
    #  ###my.glmm<-stan_glmer(y~relevel(treatment, ref = Treat.best) + (1 | site:subgroup), data = nma_data, prior = prior,
    #     #                 prior_intercept = prior_int, family = binomial(link = "logit"), 
    #     #                 cores = 1, refresh=0) 
    #  mof<-posterior_interval(my.glmm, prob = 0.95)
    #  std.err<-my.glmm$ses[2:no_treatment]
    #  out<-cbind(Estimate=my.glmm$coefficients[2:no_treatment],
    #             model_var=std.err^2,
    #             z=my.glmm$coefficients[2:no_treatment]/std.err,
    #             LL.1=mof[2:no_treatment, 1],
    #             UL.1=mof[2:no_treatment, 2])
    #  out[which(abs(out[,1])>12),]<-NA #parameter not converged is set to NA
    #}
    
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
      #fit.coeff <- append(out[,1], 0, after=(Treat.best-1))
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
