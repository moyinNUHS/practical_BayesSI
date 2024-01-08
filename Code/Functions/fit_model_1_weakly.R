## --------------------------------------------------------------------------- #
# model 1 extension non-informative prior: fit fixed effect model to all data  #
# ---------------------------------------------------------------------------- #
 
 fit_model_1_weakly <- function(nma_data, 
                               Trial_Treat_lab_vec) {

  # number of patterns
  no_p <- no_pattern
  
  warn <- NULL 
  # define priors
  loc_NI <- 0
  scale_NI <- 5
  prior <- student_t(df = 7, loc_NI, scale_NI)
  prior_int <- student_t(df = 7, loc_NI, scale_NI)
  
  # model 
  my.glm <- 
    myTryCatch(
      stan_glm(
        y ~ -1 + treatment + subgroup,
        data = nma_data,
        prior = prior,
        prior_intercept = prior_int,
        family = binomial(link = "logit"),
        chains = 8,  #default is 4 chains
        iter = 2000, #default is 2000
        cores = 1,
        refresh = 0
      )) 
  
  if (is.null(my.glm$error))
    #if do not have an error, model is fitted
  {
    # extract model output 
    my.glmm <- my.glm[[1]]
    
    # Find Type 1 error no correction 
    out = glm_output_stan_nocorrection(model =  my.glmm, no_treatment)
   if (!is.null(my.glm$warn)){
      warn <- my.glm$warn
    }
  } else { # if there is error, do not fit model 
    out <- matrix(rep(NA, (no_treatment) * 5), nrow = no_treatment, ncol = 5)
     out[1,5] <- my.glm$error
    warn <- my.glm$error
  }
  
  # for each subgroup, prepare the coefficients to identify rankings
  rank.v = rank.v.mat(no_p, Trial_Treat_lab_vec, my.glm, out)
  
  return(list(contrast.est = out, 
              ranking = rank.v,
              warn = warn))
}
