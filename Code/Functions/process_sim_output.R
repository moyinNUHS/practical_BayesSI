### process and summarise simulation output

process_sim_output <- function(output_replication, R, no_treatment, no_pattern, pattern, phi_v, lambda) {
  
  ### output_replication 
  # has length = number of iterations
  # within each iteration = contains list of simulation outputs
  
  no_comparison <- sapply(1:no_pattern, function(i) {
      length(pattern[[i]]) - 1
    })
  # for each pattern, the number of pairwise comparisons fixing a reference treatment
  
  ### summarise failed models - sum of failed models for each iteration
  ### col = patterns, row = methods
  identify_fail.list = map(output_replication, "identify_fail") # extract `identify_fail` in each iteration
  methodA_fail_no = Reduce('+', identify_fail.list) # add failed models from all iterations
  
  ### put estimators and variance for each method into a list
  method.names = names(output_replication[[1]])[grep('method', names(output_replication[[1]]))] # get vector of method names 
  estimator_all = model_var_all = list()
  for (m in method.names){
    df_list = map(output_replication, m)
    
    # estimators 
    rows = lapply(df_list, function(x) {
      x['Estimate']
    })
    estimator_all[[m]] = do.call(rbind, rows)
    
    # variance 
    rows = lapply(df_list, function(x) {
      x['model_var']
    })
    model_var_all[[m]] = do.call(rbind, rows)
  }
  
  ### get properties of estimators 
  estimator_property = lapply(1:(no_treatment - 1), function(x) {
    estimator_prop(x, output_replication, method.names, phi_v, R)
  })
  
  names(estimator_property) <-
    sapply(2:no_treatment, function(i)
      paste0("phi", i))
  # estimator_property has length = total number of treatments - 1 (reference treatment)
  # within each treatment contains warnings, and properties for all methods
  
  ### get properties for each method 
  method.property.list = map(estimator_property, "property")
  method.property = list()
  for (m in method.names){
    m.property = lapply(method.property.list, function(x) {x[ ,m]})
    method.property[[m]] = do.call(rbind, m.property)
  }
  
  # get suggested treatment for each pattern
  suggested_treatment_each <- lapply(1:no_pattern, suggested_treatment, R, output_replication)
  
  names(suggested_treatment_each) <-
    sapply(1:no_pattern, function(i)
      paste0("pattern", i))
  
  # performance of each method
  indicator.names.all = names(output_replication[[1]]$performance_m)
  indicator.names = indicator.names.all[-which(indicator.names.all == 'nearbest_treatment_10')]
  ex_performance_out <- lapply(indicator.names, function(j) {
    sapply(1:no_pattern, function(i)
      ex_performance(j, i, output_replication, R))
  })
  names(ex_performance_out) <- indicator.names
  
  estimand2 <- do.call(cbind, lapply(ex_performance_out, function(x) {
      apply(x, 1, function(y) {
        sum(y * lambda)
      })
    }))
  
  estimand2_MCSE <- sqrt(estimand2[, 3:6] * (1 - estimand2[, 3:6]) / R)
  
  
  all_diff_min <- lapply(1:R, function(z) {
      output_replication[[z]]$performance_m$diff_min
    })
  
  mortality_gain <- do.call(rbind, lapply(1:R, function(z) {
    apply(output_replication[[z]]$performance_m$mortality_gain, 1, function(x) {
      sum(x * lambda)
    })
  }))
  
  mortality_gain_ratio <- do.call(rbind, lapply(1:R, function(z) {
    apply(output_replication[[z]]$performance_m$mortality_gain_ratio, 1, function(x) {
      sum(x * lambda)
    })
  }))
  
  estimand2_MCSE <-
    cbind(Mortaliy_ratio = apply(mortality_gain_ratio, 2, function(x) {
      sqrt(var(x[complete.cases(x)]) / length(x[complete.cases(x)]))
    }), estimand2_MCSE)
  
  estimand2_MCSE <- cbind(Mortaliy = apply(mortality_gain, 2, function(x) {
    sqrt(var(x[complete.cases(x)]) / length(x[complete.cases(x)]))
  }), estimand2_MCSE)
  
  
  # number of participants randomised to treatment 
  size_per_arm <- size_pattern / (no_comparison + 1)
  
  each_t <- function(k) {
    com_size_each <- function(i) {
      v <- pattern[[i]]
      m <- cbind(v, rep(size_per_arm[i], length(v)))
      
      #m<-size_per_gp[[i]]
      
      tv <- which(m[, 1] == k)
      if (length(tv) == 0) {
        0
      } else {
        m[tv, 2]
      }
      
    }
    sum(sapply(1:length(pattern), com_size_each))
  }
  
  ex_arm_size <- sapply(1:no_treatment, each_t)
  
  t_freq <- t(sapply(1:R, function(r) output_replication[[r]]$freq_t))
  
  freq_treatment <- apply(t_freq, 2, summary)[c(1, 3, 4, 6),]
  
  list(
    method.property = method.property,
    ex_performance_out = ex_performance_out,
    suggested_treatment_each = suggested_treatment_each,
    estimator_all = estimator_all,
    #model_var_all=model_var_all,
    #all_diff_min=all_diff_min,
    method_fail_no = methodA_fail_no,
    estimand2 = estimand2,
    estimand2_MCSE = estimand2_MCSE,
    ex_arm_size = ex_arm_size,
    overall_size = freq_treatment,
    Pattern = pattern
  )
  
}
