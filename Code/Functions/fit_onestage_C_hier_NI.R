# ------------------------------------------------------------------------------------------------------ #
# method C extension non-informative prior: fit one step model to all data using hierarchical structure  #
# ------------------------------------------------------------------------------------------------------ #
fit_onestage_C_hier_NI <- function(alldata, 
                                   alternative = 'two-sided', 
                                   p = 0.05,
                                   type1correction = T) {
  # number of patterns
  no_p <- no_pattern
  
  # define priors
  loc_NI <- 0
  scale_NI <- 5
  prior <- student_t(df = 7, loc_NI, scale_NI)
  prior_int <- student_t(df = 7, loc_NI, scale_NI)
  
  # put trial data in a dataframe - outcome, treatment, pattern/subgroup
  nma_data <- data.frame(
    y = unlist(alldata[1, ]),
    treatment = factor(unlist(alldata[2, ]), levels = sort(unique(unlist(
      alldata[2, ]
    )))),
    subgroup = factor(unlist(alldata[4, ]))
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
        cores = 1,
        refresh = 0
      )
    )
  ###my.glm<-myTryCatch(stan_glmer(y~treatment + (1 | site:subgroup), data = nma_data, prior = prior,
  #prior_intercept = prior_int, family = binomial(link = "logit"),
  #cores = 1, refresh=0) )
  
  if (is.null(my.glm$error)) #if do not have an error, model is fitted
  {
    my.glmm <- my.glm[[1]]
    #Treat.best<-which.min(c(0, my.glmm$coefficients[2:no_treatment]))
    #if (Treat.best==1){
    
    if (type1correction == T) {
      
      out = glm_output_stan_bonferr(model =  my.glmm, p, no_treatment)
      
    } else {
      
      out = glm_output_stan_nocorrection(model =  my.glmm, p, no_treatment)
    }
    
    
    #} else {
    #  my.glmm<-stan_glmer(y~relevel(treatment, ref = Treat.best) + (1 | subgroup), data = nma_data, prior = prior,
    #                                 prior_intercept = prior_int, family = binomial(link = "logit"),
    #                                 cores = 1, refresh=0)
    #  ###my.glmm<-stan_glmer(y~relevel(treatment, ref = Treat.best) + (1 | site:subgroup), data = nma_data, prior = prior,
    #  #prior_intercept = prior_int, family = binomial(link = "logit"),
    #  #cores = 1, refresh=0)
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
