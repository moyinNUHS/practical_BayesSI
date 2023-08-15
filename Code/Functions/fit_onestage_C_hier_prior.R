fit_onestage_C_hier_prior <- function(alldata_prior,
                                      alldata,
                                      Scale, 
                                      alternative = 'two-sided', 
                                      p = 0.05,
                                      type1correction = T) {
  # number of patterns
  no_p <- no_pattern
  
  # generate current trial data using prior historical data 
  # put historical data in a dataframe - outcome, treatment, pattern/subgroup
  nma_data_prior <- data.frame(
    y = unlist(alldata_prior[1,]),
    treatment = factor(unlist(alldata_prior[2,]), levels = sort(unique(
      unlist(alldata_prior[2,])
    ))),
    subgroup = factor(unlist(alldata_prior[4,]))#,
    #site=factor(unlist(alldata_prior[5,]))
  )
  
  my.glm_prior <-
    myTryCatch(glmer(
      y ~ treatment + (1 | subgroup),
      family = "binomial",
      data = nma_data_prior
    ))
  ###my.glm<-myTryCatch(glmer(y~treatment + (1 | site:subgroup),family="binomial",data=nma_data) )
  
  if (!is.null(my.glm_prior$error))
  {
    # if there is error/warning, change optimizer
    my.glm_prior <-
      myTryCatch(glmer(
        y ~ treatment + (1 | subgroup),
        family = "binomial",
        data = nma_data_prior, control=glmerControl(optimizer="bobyqa")
      ))
    if (!is.null(my.glm_prior$error)){
      
      # if there is still error/warning, change to fixed effect model 
      my.glm <-
        myTryCatch(glm(
          y ~ treatment + subgroup,
          family = "binomial",
          data = nma_data_prior
        ))
      
    }  
  }
  my.glm_prior_coeff <- fixef(my.glm_prior$value)
  
  prior <-
    normal(
      location = my.glm_prior_coeff[2:no_treatment],
      scale = rep(Scale, (no_treatment - 1)),
      autoscale = TRUE
    )
  prior_int <-
    normal(location = my.glm_prior_coeff[1],
           scale = Scale,
           autoscale = TRUE)
  
  # put current trial data in a dataframe - outcome, treatment, pattern/subgroup
  nma_data <- data.frame(
    y = unlist(alldata[1,]),
    treatment = factor(unlist(alldata[2,]), levels = sort(unique(unlist(
      alldata[2,]
    )))),
    subgroup = factor(unlist(alldata[4,]))
    #site=factor(unlist(alldata[5,]))
  )
  
  # model 
my.glm <- 
  myTryCatch(
    stan_glmer(
    y ~ treatment + (1 | subgroup),
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

  
  ### my.glm<-myTryCatch(stan_glmer(y~treatment + (1 | site:subgroup), data = nma_data, prior = prior,
  #                           prior_intercept = prior_int, family = binomial(link = "logit"),
  #                           cores = 1, refresh=0) )
  
  if (is.null(my.glm$error))
    #if do not have an error, model is fitted
  {
    my.glmm <- my.glm[[1]]
    # Treat.best<-which.min(c(0, my.glmm$coefficients[2:no_treatment]))
    # if (Treat.best==1){
    
    if (type1correction == T) {
      
      out = glm_output_stan_bonferr(model =  my.glmm, p, no_treatment)
      
    } else {
      
      out = glm_output_stan_nocorrection(model =  my.glmm, p, no_treatment)
    }
    
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
    
  } else {
    # if there is error, do not fit model
    out <-
      matrix(rep(NA, (no_treatment - 1) * 5), nrow = no_treatment - 1, ncol = 5)
    out[1, 5] <- my.glm$error[1]$message
    
  }
  
  
  # for each subgroup, prepare the coefficients to identify rankings
  rank.v = rank.v.mat(no_p, alldata, my.glm, out)
  
  return(list(contrast.est = out, ranking = rank.v))
}
