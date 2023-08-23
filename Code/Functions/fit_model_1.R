# ---------------------------------------------------------------- #
# method 1 original: fit fixed effect model to current trial data  # 
# ---------------------------------------------------------------- #

fit_model_1 <- function(nma_data, 
                        alldata, 
                        alt_hypothesis = 'two.sided', 
                        type1correction = T){
  
  # number of patterns
  no_p <- no_pattern
  
  # model 
  my.glm <- myTryCatch(glm(y ~ treatment + subgroup, family = "binomial", data = nma_data) )
  
  if( is.null(my.glm$error) ) #if do not have an error, model is fitted
  { 
    
    # extract model output 
    my.glmm <- my.glm[[1]]

    # Treat.best<-which.min(c(0, coefficients(mof)[2:no_treatment]))
    # if (Treat.best==1){
    
    # Type 1 error correction 
    if (type1correction == T) {
      
      out = glm_output_dunnett(my.glmm)

    } else {
      
      out = glm_output_nocorrection(my.glmm)
      
    }

    # comparison between no adjustmentfor multiplicity vs with adjustment with Dunnett Stepdown
    ## estimates remain the same between out and out.naive 
    ## model variance and confidence intervals inflated 
    
    # using best treatment as the reference level 
    #  } else {
    #    my.glmm<-glm(y~relevel(treatment, ref = Treat.best) + subgroup,family="binomial",data=nma_data)
    ###    my.glmm<-glmer(y~relevel(treatment, ref = Treat.best) + subgroup + (1 | site),family="binomial",data=nma_data)
    #    mof<-summary(my.glmm)
    #    std.err<-sqrt(diag(vcov(mof))[2:no_treatment]) 
    #    out<-cbind(Estimate=coefficients(mof)[2:no_treatment],
    #               model_var=std.err^2,
    #               z=coefficients(mof)[2:no_treatment]/std.err,
    #               LL=coefficients(mof)[2:no_treatment] - q.val  * std.err,
    #               UL=coefficients(mof)[2:no_treatment] + q.val  * std.err)
    #  }
    
  } else { 
    
    # if there is error, do not fit model
    out <- matrix(rep(NA,(no_treatment-1)*5), nrow = no_treatment-1, ncol = 5 )
    out[1,5] <- my.glm$error[1]$message
    
  } 
  
  # gives a matrix where 
  # 1st row = best treatments 
  # 2nd row indicates 1 if any models did not fit 
  rank.v = rank.v.mat(no_p, alldata, my.glm, out)
  
  return(list(contrast.est = out, 
              ranking = rank.v))
}
