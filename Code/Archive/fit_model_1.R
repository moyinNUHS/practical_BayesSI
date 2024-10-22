# ------------------------------------------------------ #
# model 1: fit fixed effect model to current trial data  # 
# ------------------------------------------------------ #

#Includes Bonferroni and Dunnett correction options

fit_model_1 <- function(nma_data, 
                        Trial_Treat_lab_vec, 
                        alt_hypothesis = 'two.sided', 
                        p = 0.2,
                        bonferr = T, 
                        dunnett = F){
  
  # number of patterns
  no_p <- no_pattern
  
  # model 
  my.glm <- myTryCatch(glm(y ~ -1 + treatment + subgroup, family = "binomial", data = nma_data) )
  
  if( is.null(my.glm$error) ) #if do not have an error, model is fitted
  { 
    # extract model output 
    my.glmm <- my.glm[[1]]
    
    # Type 1 error correction 
    if (dunnett == T) {
      out = glm_output_dunnett(my.glmm)
    } else if (bonferr == T) {
      out = glm_output_bonferr(model = my.glmm, p, no_treatment)
    } else {
      out = glm_output_nocorrection(my.glmm, p)
    }
    
  } else { 
    # if there is error, do not fit model
    out <- matrix(rep(NA,(no_treatment)*5), nrow = no_treatment, ncol = 5 )
    out[1,5] <- my.glm$error[1]$message
  } 
  
  # gives a matrix where 
  # 1st row = best treatments 
  # 2nd row indicates 1 if any models did not fit 
  rank.v = rank.v.mat(no_p, Trial_Treat_lab_vec, my.glm, out)
  
  return(list(contrast.est = out, 
              ranking = rank.v))
}
