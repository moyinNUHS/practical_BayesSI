# --------------------------------------------------------------------------- #
# model 2: fit random effects model to all data using hierarchical structure  #
# --------------------------------------------------------------------------- #
fit_model_2 <- function(nma_data, 
                        alldata,
                        alternative = 'two-sided', 
                        p = 0.05,
                        bonferr = T, 
                        dunnett = F) {
  # number of patterns
  no_p <- no_pattern
  
  # logistic regression
  my.glm <-
    myTryCatch(glmer(
      y ~ -1 + treatment + (1 | subgroup),
      family = "binomial",
      data = nma_data
    ))
  
if (!is.null(my.glm$error)) {
  # if there is error, change optimizer
  my.glm <-
    myTryCatch(glmer(
      y ~ -1 + treatment + (1 | subgroup),
      family = "binomial",
      data = nma_data, control=glmerControl(optimizer="bobyqa")
      ))
}
  
  # If there is still error, or warning about singularity - indicate null random effects, use fixed effect model
  if (!is.null(my.glm$error)|!is.null(my.glm$warning)){
    my.glm <-
      myTryCatch(glm(
        y ~ -1 + treatment + subgroup,
        family = "binomial",
        data = nma_data
      ))
  }  
  
  if (is.null(my.glm$error)) {
    #if do not have an error, model is fitted
    my.glmm <- my.glm[[1]]
    
    # Type 1 error correction
    if (dunnett == T) {
      out = glm_output_dunnett(my.glmm)
    } else if (bonferr == T) {
      out = glm_output_bonferr(model = my.glmm, p, no_treatment)
    } else {
      out = glm_output_nocorrection(my.glmm)
    }
    
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
