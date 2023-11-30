# ----------------------------------------------------------------------------------------------- #
# model 1 extension informative prior: fit fixed model to all data using historical data as prior #
# ----------------------------------------------------------------------------------------------- #

fit_model_1_prior <- function(nma_data_prior, 
                              nma_data, 
                              Trial_Treat_lab_vec, 
                              Scale, 
                              alternative = 'two-sided', 
                              p = 0.2,
                              bonferr = T) {
  # number of patterns
  no_p <- no_pattern
  
  my.glm_prior <-
    glm(y ~ -1 + treatment + subgroup, family = "binomial", data =
          nma_data_prior)
  my.glm_prior_coeff <- my.glm_prior$coefficients
  
  prior <-
    normal(location = my.glm_prior_coeff[1:(no_treatment + no_p - 1)], 
           scale = rep(Scale, (no_treatment + no_p - 1))) 
  
  # logistic regression
  my.glm <- 
  myTryCatch(
    stan_glm(
    y ~ -1 + treatment + subgroup,
    data = nma_data,
    prior = prior,
    # prior_intercept = prior_int,
    family = binomial(link = "logit"),
    chains = 8,  
    iter = 2000, 
    cores = 1,
    refresh = 0
  )) 
  
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
  
  return(list(contrast.est = out, 
              ranking = rank.v))
}
