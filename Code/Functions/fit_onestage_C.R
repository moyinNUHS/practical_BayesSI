# ----------------------------------------- #
# method C original: fit one step model to current trial data  # 
# ----------------------------------------- #

fit_onestage_C <- function(alldata, t1_error = 0.05, alt_hypothesis = 'two.sided'){
  
  no_p<-no_pattern
  
  # put trial data in a dataframe - outcome, treatment, pattern/subgroup
  nma_data <- data.frame(y = unlist(alldata[1,]),
                         treatment = factor(unlist(alldata[2,]), levels = sort(unique(unlist(alldata[2,])))),
                         subgroup = factor(unlist(alldata[4,]))#, 
                         #site=factor(unlist(alldata[5,]))
  )
  
  my.glm <- myTryCatch(glm(y~treatment + subgroup,family = "binomial",data = nma_data) )
  
  # data.frame(unadj = round(summary(my.glm)$coeff[2:4,4], 5), 
  #            dunnett = round(summary(dunnett_test)$test$pvalues, 5), 
  #            stepdowndunnett = round(summary(stepdown)$test$pvalues, 5))
  
  q.val <- qnorm(1 - t1_error/2) 
  
  if( is.null(my.glm$error) ) #if do not have an error, model is fitted
  { 
    
    # extract model output 
    my.glm<-my.glm[[1]]
    mof<-summary(my.glm)
    
    # Treat.best<-which.min(c(0, coefficients(mof)[2:no_treatment]))
    # if (Treat.best==1){
    # Dunnett test 
    dunnett_test <- glht(model = my.glm, 
                         linfct = mcp(treatment = "Dunnett"),
                         alternative = alt_hypothesis)
    stepdown <- summary(dunnett_test, test = adjusted(type = "free")) # Step-down Dunnett test
    
    # get standard error
    std.err <- stepdown$test$sigma # inflated std error to account for multiplicity
    # mof$coefficients[,2]
    # std.err<-sqrt(diag(vcov(mof))[2:no_treatment]) 
    
    out <- cbind(Estimate = stepdown$test$coefficients,
                 model_var = std.err^2,
                 z = stepdown$test$tstat,
                 LL = stepdown$test$coefficients - q.val  * std.err,
                 UL = stepdown$test$coefficients + q.val  * std.err)
    
    out[which(abs(out[,1])>12),]<-NA #parameter not converged is set to NA 
    
    # out<-cbind(Estimate=coefficients(mof)[2:no_treatment],
    #          model_var=std.err^2,
    #          z=coefficients(mof)[2:no_treatment]/std.err,
    #          LL=coefficients(mof)[2:no_treatment] - q.val  * std.err,
    #          UL=coefficients(mof)[2:no_treatment] + q.val  * std.err)
    
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
  
  rank.v <- sapply(1:no_p, prep.coeff)
  
  colnames(rank.v) <- sapply(1:no_pattern, function(i)paste("pattern", i))
  row.names(rank.v) <- c("suggested treatment", "model.not.fit")
  
  return(list(contrast.est = out, 
              ranking = rank.v) )
}
