# ---------------------------------------------------------------------- #
# method C: fit one step model to all data using hierarchical structure  #
# ---------------------------------------------------------------------- #
fit_onestage_C_hier <- function(alldata,
                                alternative = 'two-sided', 
                                p = 0.05,
                                type1correction = T) {
  # number of patterns
  no_p <- no_pattern
  
  # put trial data in a dataframe - outcome, treatment, pattern/subgroup
  nma_data <- data.frame(
    y = unlist(alldata[1, ]),
    treatment = factor(unlist(alldata[2, ]), levels = sort(unique(unlist(
      alldata[2, ]
    )))),
    subgroup = factor(unlist(alldata[4, ]))#,
    #  site=factor(unlist(alldata[5,]))
  )
  
  # logistic regression
  my.glm <-
    myTryCatch(glmer(
      y ~ treatment + (1 | subgroup),
      family = "binomial",
      data = nma_data
    ))
  ###my.glm<-myTryCatch(glmer(y~treatment + (1 | site:subgroup),family="binomial",data=nma_data) )
  
  
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
  rank.v <- sapply(1:no_p, prep.coeff, alldata)
  
  colnames(rank.v) <-
    sapply(1:no_pattern, function(i)
      paste("pattern", i))
  row.names(rank.v) <- c("suggested treatment", "model.not.fit")
  
  return(list(contrast.est = out, ranking = rank.v))
}
