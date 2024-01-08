# ------------------------------------------------------ #
# model 1: fit fixed effect model to current trial data  # 
# ------------------------------------------------------ #

fit_model_1 <- function(nma_data, 
                        Trial_Treat_lab_vec){
  
  # number of patterns
  no_p <- no_pattern
  warn <- NULL 
  # model 
  my.glm <- myTryCatch(glm(y ~ -1 + treatment + subgroup, family = "binomial", data = nma_data) )
  
  if( is.null(my.glm$error) ) #if do not have an error, model is fitted
  { 
    # extract model output 
    my.glmm <- my.glm[[1]]
    
    # Find Type 1 error no correction 
    out = glm_output_nocorrection(my.glmm)
    
    if (!is.null(my.glm$warn)){
      warn <- my.glm$warn
    }
    
  } else { 
    # if there is error, do not fit model
    out <- matrix(rep(NA,(no_treatment)*5), nrow = no_treatment, ncol = 5 )
    out[1,5] <- my.glm$error
    warn <- my.glm$error
  } 
  
  
  # gives a matrix where 
  # 1st row = best treatments 
  # 2nd row indicates 1 if any models did not fit 
  rank.v = rank.v.mat(no_p, Trial_Treat_lab_vec, my.glm, out)
  
  #Return model coefficients and predicted best treatments per pattern
  return(list(contrast.est = out, 
              ranking = rank.v,
              warn = warn))
}
