# -------------------------------------------------------------------------------------------------------- #
# model 2 extension informative prior: fit random effects model to all data using historical data as prior #
# -------------------------------------------------------------------------------------------------------- #
fit_model_2_prior <- function(nma_data_prior, 
                              nma_data,
                              Trial_Treat_lab_vec,
                              Scale, 
                              alternative = 'two-sided', 
                              p = 0.2,
                              bonferr = T) {
  # number of patterns
  no_p <- no_pattern
  
  my.glm_prior <-
    myTryCatch(glmer(
      y ~ -1 + treatment + (1 | subgroup),
      family = "binomial",
      data = nma_data_prior
    ))
  
  if (!is.null(my.glm_prior$error))
  {
    # if there is error, change optimizer
    my.glm_prior <-
      myTryCatch(glmer(
        y ~ -1 + treatment + (1 | subgroup),
        family = "binomial",
        data = nma_data_prior, 
        control=glmerControl(optimizer="bobyqa")
      ))
  }
  
  if (!is.null(my.glm_prior$error) | !is.null(my.glm_prior$warning)){
    # if there is still error/warning, change to fixed effect model 
    my.glm_prior <-
      myTryCatch(glm(
        y ~ -1 + treatment + subgroup,
        family = "binomial",
        data = nma_data_prior
      ))
    my.glm_prior_coeff <- my.glm_prior$value$coefficients
    
    prior <-
      normal(
        location = my.glm_prior_coeff[1:(no_treatment + no_p - 1)],
        scale = rep(Scale, (no_treatment + no_p - 1)),
        autoscale = TRUE
      )
    
    # model 
    my.glm <- 
      myTryCatch(
        stan_glm(
          y ~ -1 + treatment + subgroup,
          data = nma_data,
          prior = prior,
          family = binomial(link = "logit"),
          chains = 8,  
          iter = 2000, 
          cores = 1,
          refresh = 0
        )) 
    
  } else {
    #If no error/warning
    my.glm_prior_coeff <- fixef(my.glm_prior$value)
    
    prior <-
      normal(
        location = my.glm_prior_coeff[1:no_treatment],
        scale = rep(Scale, (no_treatment)),
        autoscale = TRUE
      )
    
    # model 
    my.glm <- 
      myTryCatch(
        stan_glmer(
          y ~ -1 + treatment + (1 | subgroup),
          data = nma_data,
          prior = prior,
          family = binomial(link = "logit"),
          chains = 8,  
          iter = 2000, 
          cores = 1,
          refresh = 0
        )) 
  }
  
  if (is.null(my.glm$error))
    #if do not have an error, model is fitted
  {
    my.glmm <- my.glm[[1]]
    
    if (bonferr == T) {
      out = glm_output_stan_bonferr(model = my.glmm, p, no_treatment)
    } else {
      out = glm_output_stan_nocorrection(model = my.glmm, p, no_treatment)
    }
    
  } else {
    # if there is error, do not fit model
    out <-
      matrix(rep(NA, (no_treatment - 1) * 5), nrow = no_treatment - 1, ncol = 5)
    out[1, 5] <- my.glm$error[1]$message
  }
  
  # for each subgroup, prepare the coefficients to identify rankings
  rank.v = rank.v.mat(no_p, Trial_Treat_lab_vec, my.glm, out)
  
  return(list(contrast.est = out, ranking = rank.v))
}
