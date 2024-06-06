# -------------------------------------------------------------------------------------------------------- #
# model 2 extension informative prior: fit random effects model to all data using historical data as prior #
# -------------------------------------------------------------------------------------------------------- #
fit_model_2_noprior <- function(
                              nma_data,
                              Trial_Treat_lab_vec,
                              Scale) {
  
  # number of patterns
  no_p <- no_pattern
  warn <- NULL 
  
    # model 
  my.glm <- 
      myTryCatch(
        stan_glmer(
          y ~ -1 + treatment + (1 | subgroup),
          data = nma_data,
          family = binomial(link = "logit"),
          chains = 8,  
          iter = 2000, 
          cores = 1,
          refresh = 0
        )) 
  
  if (is.null(my.glm$error))
    #if do not have an error, model is fitted
  {
    # extract model output 
    my.glmm <- my.glm[[1]]
    
    # Find Type 1 error no correction 
    
    out = glm_output_stan_nocorrection(model = my.glmm, no_treatment)
    
    if (!is.null(my.glm$warn)){
      warn <- my.glm$warn
    }
  } else {
    # if there is error, do not fit model
    out <-
      matrix(rep(NA, (no_treatment - 1) * 5), nrow = no_treatment - 1, ncol = 5)
    out[1,5] <- my.glm$error
    warn <- my.glm$error
  }
  
  # for each subgroup, prepare the coefficients to identify rankings
  rank.v = rank.v.mat(no_p, Trial_Treat_lab_vec, my.glm, out)
  
  return(list(contrast.est = out, 
              ranking = rank.v,
              warn = warn))
}

