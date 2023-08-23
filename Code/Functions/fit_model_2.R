# --------------------------------------------------------------------------- #
# model 2: fit random effects model to all data using hierarchical structure  #
# --------------------------------------------------------------------------- #
fit_model_2 <- function(nma_data, 
                        alldata,
                        alternative = 'two-sided', 
                        p = 0.05,
                        type1correction = T) {
  # number of patterns
  no_p <- no_pattern
  
  # logistic regression
  my.glm <-
    myTryCatch(glmer(
      y ~ treatment + (1 | subgroup),
      family = "binomial",
      data = nma_data
    ))
  ###my.glm<-myTryCatch(glmer(y~treatment + (1 | site:subgroup),family="binomial",data=nma_data) )
  
if (!is.null(my.glm$error)) {
  # if there is error, change optimizer
  my.glm <-
    myTryCatch(glmer(
      y ~ treatment + (1 | subgroup),
      family = "binomial",
      data = nma_data, control=glmerControl(optimizer="bobyqa")
      ))
}
  
  # If there is still error, or warning about singularity - indicate null random effects, use fixed effect model
  if (!is.null(my.glm$error)|!is.null(my.glm$warning)){
    
    my.glm <-
      myTryCatch(glm(
        y ~ treatment + subgroup,
        family = "binomial",
        data = nma_data
      ))
    
  }  
  
  if (is.null(my.glm$error))
    #if do not have an error, model is fitted
  {
    my.glmm <- my.glm[[1]]
    
    # Type 1 error correction
    
    #########
    #THIS IS NOT WORKING YET!!!
    #########
    if (type1correction == T) {
      out = glm_output_dunnett(my.glmm)
      
    } else {
      out = glm_output_nocorrection(my.glmm)
      
    }
    
    # } else {
    #   my.glmm<-glmer(y~relevel(treatment, ref = Treat.best) + (1 | subgroup),family="binomial",data=nma_data)
    #   ###my.glmm<-glmer(y~relevel(treatment, ref = Treat.best) + (1 | site:subgroup),family="binomial",data=nma_data)
    #   mof<-summary(my.glmm)
    #   std.err<-sqrt(diag(vcov(mof))[2:no_treatment])
    #   out<-cbind(Estimate=coefficients(mof)[2:no_treatment],
    #              model_var=std.err^2,
    #              z=coefficients(mof)[2:no_treatment]/std.err,
    #              LL=coefficients(mof)[2:no_treatment] - q.val  * std.err,
    #              UL=coefficients(mof)[2:no_treatment] + q.val  * std.err)
    #   out[which(abs(out[,1])>12),]<-NA #parameter not converged is set to NA
    # }
    
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
