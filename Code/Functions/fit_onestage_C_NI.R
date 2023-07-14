## ------------------------------------------------------------------------- #
# method C extension non-informative prior: fit one step model to all data  #
# ------------------------------------------------------------------------- #

fit_onestage_C_NI <- function(alldata, 
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
    treatment = factor(unlist(alldata[2, ]),
                       levels = sort(unique(unlist(
                         alldata[2, ]
                       )))),
    subgroup = factor(unlist(alldata[4, ]))
    #site=factor(unlist(alldata[5,]))
  )
  
  # model 
  my.glm <-
    myTryCatch(
      stan_glm(
        y ~ treatment + subgroup,
        data = nma_data,
        prior = prior,
        prior_intercept = prior_int,
        family = binomial(link = "logit"),
        cores = 1,
        refresh = 0
      )
    )
  
  ### my.glm<-myTryCatch(stan_glmer(y~treatment + subgroup + (1 | site), data = nma_data, prior = prior,
  #                           prior_intercept = prior_int, family = binomial(link = "logit"),
  #                          cores = 1, refresh=0) )
  #use of default priors in stan_glm are non-informative
  
  if (is.null(my.glm$error))
    #if do not have an error, model is fitted
  {
    # extract model output 
    my.glmm <- my.glm[[1]]
    #Treat.best<-which.min(c(0, my.glmm$coefficients[2:no_treatment]))
    #if (Treat.best==1){
    
    if (type1correction == T) {
      # Bonferroni correction
      adjusted.p <- p / no_treatment
      
      # get posterior intervals with adjusted p 
      mof <- posterior_interval(my.glmm, prob = 1 - adjusted.p)
      
      # get standard errors using posterior samples 
      posterior <- as.matrix(my.glmm)
      q.val <- qnorm(1 - adjusted.p / 2) # z score 
      std.err <- apply(posterior, 2, function(x) quantile(x, (1 - adjusted.p) / 2) / q.val)
      
      # output 
      out <- cbind(Estimate = my.glmm$coefficients[2:no_treatment],
                   model_var = std.err[2:no_treatment]^2,
                   z = q.val,
                   LL = mof[2:no_treatment, 1],
                   UL = mof[2:no_treatment, 2])
      
      out[which(abs(out[,1])>12),] <- NA #parameter not converged is set to NA 
      
    } else {
      
      # get posterior intervals with unadjusted p 
      mof.naive <- posterior_interval(my.glmm, prob = 1 - p)
      
      # get standard error
      std.err.naive <- my.glmm$ses[2:no_treatment]
      
      out <- cbind(
        Estimate = my.glmm$coefficients[2:no_treatment],
        #get_estimates(my.glmm, centrality = "mean")[2:no_treatment, 2], #for mean instead of median
        model_var = std.err.naive ^ 2,
        z = abs(my.glmm$coefficients[2:no_treatment] / std.err.naive),
        #get_estimates(my.glmm, centrality = "mean")[2:no_treatment, 2]/std.err,
        LL = mof.naive[2:no_treatment, 1],
        UL = mof.naive[2:no_treatment, 2]
      )
    }
    
  } else { # if there is error, do not fit model
    
    out <- matrix(rep(NA, (no_treatment - 1) * 5), nrow = no_treatment - 1, ncol = 5)
    out[1, 5] <- my.glm$error[1]$message
    out[which(abs(out[, 1]) > 12), ] <- NA #parameter not converged is set to NA
    # } else {
    #   my.glmm<-stan_glm(y~relevel(treatment, ref = Treat.best) + subgroup, data = nma_data, prior = prior,
    #                     prior_intercept = prior_int, family = binomial(link = "logit"),
    #                     cores = 1, refresh=0)
    ###my.glmm<-stan_glmer(y~relevel(treatment, ref = Treat.best) + subgroup + (1 | site), data = nma_data, prior = prior,
    #prior_intercept = prior_int, family = binomial(link = "logit"),
    #cores = 1, refresh=0)
    #   mof<-posterior_interval(my.glmm, prob = 0.95)
    #   std.err<-my.glmm$ses[2:no_treatment]
    #   out<-cbind(Estimate=my.glmm$coefficients[2:no_treatment],
    #              model_var=std.err^2,
    #              z=my.glmm$coefficients[2:no_treatment]/std.err,
    #              LL.1=mof[2:no_treatment, 1],
    #              UL.1=mof[2:no_treatment, 2])
    #   out[which(abs(out[,1])>12),]<-NA #parameter not converged is set to NA
    
  }
  
  # for each subgroup, prepare the coefficients to identify rankings
  rank.v <- sapply(1:no_p, prep.coeff)
  colnames(rank.v) <-
    sapply(1:no_pattern, function(i)
      paste("pattern", i))
  row.names(rank.v) <- c("suggested treatment", "model.not.fit")
  
  return(list(contrast.est = out, ranking = rank.v))
}
