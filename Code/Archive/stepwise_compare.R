## Stepwise comparison of treatment effects 

## NOT WORKING BECAUSE DECISION MADE TO TAKE OUT INTERCEPT INSTEAD OF USING THIS METHOD

stepwise_compare <- function(model, no_treatment, p_diff = 0.05) {
  
  coefs = summary(model$value)$coef[1:no_treatment,] # get coefficients 
  
  ## fix names 
  tx_names = paste0('T', 1:no_treatment)
  rownames(coefs) = tx_names
  
  ## variance matrix 
  vc = vcov(model$value)[1:no_treatment, 1:no_treatment]
  dimnames(vc) = list(tx_names, tx_names)
  
  ## find reference treatment
  ests = coefs[, 'Estimate']  
  ests[2:no_treatment] = ests[2:no_treatment] + coefs['T1', 'Estimate']
  ref_tx = names(ests[1]) ## find the reference treatment in the regression
  
  ## create list for comparison 
  rangelab_tb_all = expand.grid(tx_names, tx_names) # possible comparisons
  rangelab_tb = rangelab_tb_all[which(rangelab_tb_all[,1] != rangelab_tb_all[,2]),] # remove rows with the same comparisons
  rangelab_tb_uniq = unique(t(apply(rangelab_tb, 1, sort))) # remove rows with the same comparisons
  
  #############################
  ## compare treatments 
  #############################
  # if one is clearly better than all the rest, stop trial for efficacy
  
  todrop = apply(rangelab_tb_uniq, 1, function(x) {
    
    if (is.element(ref_tx, c(x[1], x[2]))) { 
      
      # if the reference treatment is among the compared treatments
      
      ## if x[1] is the reference treatment, take x[2] 
      ## if x[1] is not the reference treatment, take x[1] 
      ref_b = ifelse(x[1] == ref_tx, x[2], x[1])
      
      ## directly use z value since the comparison includes the reference treatment 
      ## use a two-sided test on whichever one is not reference
      ## i.e. is the compared treatment statistically different from reference treatment(by smaller than p_diff)
      pbest = 2*pnorm(-abs(coefs[ref_b,'z value']), 0, 1, lower.tail = T) # cumulative distribution function of a standard normal distribution
      # i.e. get back a probability given the z value
      
    }  else { 
      
      # if the reference treatment is NOT within the compared treatments
      
      ## need correlation matrix to compare 
      vardiff.best = vc[x[1], x[1]] + vc[x[2], x[2]] - 2*vc[x[1], x[2]]
      Zbest = diff(ests[c(x[1], x[2])]) / sqrt(vardiff.best)
      pbest = 2 * pnorm(-abs(Zbest), 0, 1, lower.tail = T)  
      
    }
    
    # if `pbest` is less than efficacy cutoff of p (corrected by Bonferroni)
    p = p_diff / nrow(rangelab_tb_uniq)
    ifelse(pbest < p, x[1], 0)
    
  })
  
  todrop_tx = todrop[grep('T', todrop)]
  names(table(todrop_tx) == (no_treatment - 1))
  
}



