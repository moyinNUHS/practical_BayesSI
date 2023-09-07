# ----------------------------------------------------------------------------------------------- #
# model 1 extension informative prior: fit fixed model to all data using historical data as prior #
# ----------------------------------------------------------------------------------------------- #

fit_model_1_prior <- function(nma_data_prior, 
                              nma_data, 
                              alldata, 
                              Scale, 
                              alternative = 'two-sided', 
                              p = 0.05,
                              bonferr = T) {
  # number of patterns
  no_p <- no_pattern
  
  my.glm_prior <-
    glm(y ~ treatment + subgroup, family = "binomial", data =
          nma_data_prior)
  my.glm_prior_coeff <- my.glm_prior$coefficients
  ### my.glm_prior<-glmer(y~treatment+ subgroup + (1 | site),family="binomial",data=nma_data_prior)
  ### my.glm_prior_coeff<-fixef(my.glm_prior)
  
  prior <-
    normal(location = my.glm_prior_coeff[2:(no_treatment + no_p - 1)], 
           scale = rep(Scale, (no_treatment + no_p - 2)))
  prior_int <-
    normal(location = my.glm_prior_coeff[1], scale = Scale)  
  
  # logistic regression
  my.glm <- 
  myTryCatch(
    stan_glm(
    y ~ treatment + subgroup,
    data = nma_data,
    prior = prior,
    prior_intercept = prior_int,
    family = binomial(link = "logit"),
    chains = 2,  
    iter = 1000, 
    cores = 1,
    refresh = 0
  )) 
  
  #If warning that samples not enough, do additional 500
  if (!is.null(my.glm$warning)){
    my.glm = myTryCatch(
      update(my.glm$value, iter = 500)
    )
  }
  #If warning that samples still not enough, do additional 500
  if (!is.null(my.glm$warning)){
    my.glm = myTryCatch(
      update(my.glm$value, iter = 500)
    )
  }
  ###my.glm<-myTryCatch(stan_glmer(y~treatment + subgroup + (1 | site), data = nma_data, prior = prior,
  #                        prior_intercept = prior_int, family = binomial(link = "logit"),
  #                       cores = 1, refresh=0) )
  
  if (is.null(my.glm$error))
    #if do not have an error, model is fitted
  {
    my.glmm <- my.glm[[1]]
    #Treat.best<-which.min(c(0, my.glmm$coefficients[2:no_treatment]))
    #if (Treat.best==1){
    
    if (bonferr == T) {
      
      out = glm_output_stan_bonferr(model = my.glmm, p, no_treatment)
      
    } else {
      
      out = glm_output_stan_nocorrection(model = my.glmm, p, no_treatment)
    }
    
    #} else{
    #   my.glmm<-stan_glm(y~relevel(treatment, ref = Treat.best) + subgroup, data = nma_data, prior = prior,
    #                    prior_intercept = prior_int, family = binomial(link = "logit"),
    #                    cores = 1, refresh=0)
    #   ###my.glmm<-stan_glmer(y~relevel(treatment, ref = Treat.best) + subgroup + (1 | site), data = nma_data, prior = prior,
    #   #prior_intercept = prior_int, family = binomial(link = "logit"),
    #   #cores = 1, refresh=0)
    #   mof<-posterior_interval(my.glmm, prob = 0.95)
    #   std.err<-my.glmm$ses[2:no_treatment]
    #   out<-cbind(Estimate=my.glmm$coefficients[2:no_treatment],
    #              model_var=std.err^2,
    #              z=my.glmm$coefficients[2:no_treatment]/std.err,
    #              LL.1=mof[2:no_treatment, 1],
    #              UL.1=mof[2:no_treatment, 2])
    #   out[which(abs(out[,1])>12),]<-NA #parameter not converged is set to NA
    # }
    
  } else {
    # if there is error, do not fit model
    out <-
      matrix(rep(NA, (no_treatment - 1) * 5), nrow = no_treatment - 1, ncol = 5)
    out[1, 5] <- my.glm$error[1]$message
    
  }
  
  # for each subgroup, prepare the coefficients to identify rankings
  rank.v = rank.v.mat(no_p, alldata, my.glm, out)
  
  return(list(contrast.est = out, 
              ranking = rank.v))
}
