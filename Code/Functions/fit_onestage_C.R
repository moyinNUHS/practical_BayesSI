# ----------------------------------------- #
# method C original: fit one step model to current trial data  # 
# ----------------------------------------- #

fit_onestage_C <- function(alldata, alt_hypothesis = 'two.sided'){
  
  no_p <- no_pattern
  
  # put trial data in a dataframe - outcome, treatment, pattern/subgroup
  nma_data <- data.frame(y = unlist(alldata[1,]),
                         treatment = factor(unlist(alldata[2,]), levels = sort(unique(unlist(alldata[2,])))),
                         subgroup = factor(unlist(alldata[4,]))#, 
                         #site=factor(unlist(alldata[5,]))
  )
  
  my.glm <- myTryCatch(glm(y ~ treatment + subgroup, family = "binomial", data = nma_data) )
  
  if( is.null(my.glm$error) ) #if do not have an error, model is fitted
  { 
    
    # extract model output 
    my.glm <- my.glm[[1]]
    
    # Treat.best<-which.min(c(0, coefficients(mof)[2:no_treatment]))
    # if (Treat.best==1){
    
    # Dunnett test 
    dunnett_test <- glht(model = my.glm, 
                         linfct = mcp(treatment = "Dunnett"),
                         alternative = alt_hypothesis)
    stepdown <- summary(dunnett_test, test = adjusted(type = "free")) # Step-down Dunnett test
    stepdown.p <- stepdown$test$pvalues # extract the p-values 
    q.val <- qnorm(1 - stepdown.p/2) 
    
    # get standard error
    std.err <- standard_error <- 1 / q.val # inflated std error to account for multiplicity
    
    out <- cbind(Estimate = stepdown$test$coefficients,
                 model_var = std.err^2,
                 z = q.val,
                 LL = stepdown$test$coefficients - q.val  * std.err,
                 UL = stepdown$test$coefficients + q.val  * std.err)
    
    out[which(abs(out[,1])>12),] <- NA #parameter not converged is set to NA 
    
    # compare without adjusting for multiplicity vs with adjustment with Dunnett Stepdown
    # mof <- summary(my.glm)
    # std.err.naive <- sqrt(diag(vcov(mof))[2:no_treatment]) 
    # t1_error = 0.05 # unadjusted
    # q.val.naive <- q.val <- qnorm(1 - t1_error/2) 
    # out.naive <- cbind(Estimate = coefficients(mof)[2:no_treatment],
    #                    model_var = std.err.naive^2,
    #                    z = coefficients(mof)[2:no_treatment]/std.err.naive,
    #                    LL = coefficients(mof)[2:no_treatment] - q.val.naive  * std.err.naive,
    #                    UL = coefficients(mof)[2:no_treatment] + q.val.naive  * std.err.naive)
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
    out[1,5]<-my.glm$error[1]$message
    
  } 
  
  # gives a matrix where 
  # 1st row = best treatments 
  # 2nd row indicates 1 if any models did not fit 
  rank.v <- sapply(1:no_p, prep.coeff)
  
  colnames(rank.v) <- sapply(1:no_pattern, function(i)paste("pattern", i))
  row.names(rank.v) <- c("suggested treatment", "model.not.fit")
  
  return(list(contrast.est = out, 
              ranking = rank.v))
}
