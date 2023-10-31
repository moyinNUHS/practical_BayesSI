get_type2 <- function(Scenario, d, .method_labs = method_labs, .all_method_names = all_method_names) {
  
  
  # number of treatment comparisons
  n_tx = nrow(d[[1]][['scenario_out']][[1]][['est_method_1']]) - 1
  
  ##################
  # collate data for contiguous groups 
  ##################
  n = parse_number(names(d))
  contig = list()
  
  for (i in n) { # for each sample size 
    
    # subset data for the sample size 
    subset_n = d[[grep(paste("=",as.character(i),sep=" "), names(d))]]$scenario_out
    
    # for each iteration, apply find_contig_grp function
    iter_conti = lapply(subset_n, function(iter){
      
      # list of all data.frames with estimates, each refers to a method
      df_list = iter[grep('est_method', names(iter))]
      df_list = lapply(df_list, function(x){rownames(x)=rownames(df_list$est_method_1_NI); return(x)})
      rows = lapply(df_list, find_contig_grp)
      conti_grp = as.data.frame(do.call(rbind, rows))
      conti_grp$n = i 
      conti_grp$method = rownames(conti_grp)
      conti_grp
      
    })
    
    contig[[i]] = do.call(rbind, iter_conti)
  }
  
  dat = do.call(rbind, contig)
  
  dat$method = factor(dat$method, 
                      levels = .all_method_names, 
                      labels = .method_labs)
  
  ################## 
  # calculate type 2 error 
  ##################
  
  if (Scenario == '1.2') {
    
    best_tx = '2'
    
    # here, type 2 error - 
    # when treatment 2 was not the best treatment 
    # hence, type 2 error condition : treatment1-treatment2 != 2, or 
    #                                 treatment2-treatment3 != 2, or  
    #                                 treatment2-treatment4 != 2 
    
    sub_best_dat = dat[, grep(paste0('treatment', best_tx), colnames(dat))]
    error = apply(sub_best_dat, 1, function(row){
      sum(row != best_tx) != 0 # error is committed when any of the comparisons do not show best_tx as the best 
    })
    
    out_raw = data.frame(n = dat$n, 
                         method = dat$method, 
                         t2error = error, 
                         type = 'Pre-defined best treatment identified as best',
                         scenario = Scenario)
    
    # calculate power per sample size (n), per method, per type using power = (1 - mean(error))
    out = out_raw %>% 
      group_by(n, method, type) %>% 
      summarise(type2error = mean(t2error))
    out$power = 1 - out$type2error
    
  } else if (Scenario %in% c('1.3', '1.4', '2.2', '2.4', '3.2', '4.1', '4.2', '4.3')){
    
    best_tx = '1'
    secbest_tx = '2'
    worst_tx = '4'
    secworse_tx = '3'
    
    # here, type 2 error - 
    # Failure to identify best treatments (terminating trial for efficacy)
    # (A) when treatment 1 was not the best treatment 
    # (B) when treatment 1 or 2 were not the best treatments
    # Failure to identify worst treatments (drop treatment for safety)
    # (C) when treatment 4 was not the worst treatment 
    # (D) when treatment 3 or 4 were not the worst treatments 
    
    
    # (A) when treatment 1 was not the best treatment 
    sub_best_dat = dat[, grep(paste0('treatment', best_tx), colnames(dat))]
    errorA = apply(sub_best_dat, 1, function(row){
      any(row != best_tx) # error is committed when any of the comparisons do not show best_tx as the best 
    })
    
    outA = data.frame(n = dat$n, 
                      method = dat$method, 
                      t2error = errorA, 
                      type = 'Pre-defined best treatment identified as best',
                      scenario = Scenario)
    
    # (B) when treatment 1 or 2 were not the best treatments
    sub_secbest_dat = dat[, grep(paste0('treatment', secbest_tx), colnames(dat))]
    error_secbest = apply(sub_secbest_dat, 1, function(row){
      any(row != secbest_tx) # error is committed when any of the comparisons do not show second best_tx as the best 
    })
    
    sub_twobest_dat = dat[, grep(paste0('treatment', c(secbest_tx, best_tx), collapse = '|'), colnames(dat))]
    error_twobest = apply(sub_twobest_dat, 1, function(row){
      row['treatmenttreatment1-treatmenttreatment2'] == 'overlap' & 
        row['treatmenttreatment1-treatmenttreatment3'] == '1' & 
        row['treatmenttreatment1-treatmenttreatment4'] == '1' & 
        row['treatmenttreatment2-treatmenttreatment3'] == '2' & 
        row['treatmenttreatment2-treatmenttreatment4'] == '2' 
    }) # error is committed when either top 2 predefined treatments were not concluded as the best 
    
    errorB = errorA & error_secbest & !error_twobest
    
    outB = data.frame(n = dat$n, 
                      method = dat$method, 
                      t2error = errorB, 
                      type = 'Either of top 2 pre-defined best treatment identified as best', 
                      scenario = Scenario)
    
    # (C) when treatment 4 was not the worst treatment 
    sub_worst_dat = dat[, grep(paste0('treatment', worst_tx), colnames(dat))]
    errorC = apply(sub_worst_dat, 1, function(row){
      sum(row %in% c(best_tx, secbest_tx, secworse_tx)) != n_tx 
    })
    
    outC = data.frame(n = dat$n, 
                      method = dat$method, 
                      t2error = errorC, 
                      type = 'Pre-defined worst treatment identified as worst', 
                      scenario = Scenario)
    
    # (D) when treatment 3 or 4 were not the worst treatments 
    sub_secworse_dat = dat[, grep(paste0('treatment', secworse_tx), colnames(dat))]
    error_secworse_isworst = apply(sub_secworse_dat, 1, function(row){
      sum(row %in% c(best_tx, secbest_tx, worst_tx)) != n_tx 
    })
    
    sub_twoworse_dat = dat[, grep(paste0('treatment', c(worst_tx, secworse_tx), collapse = '|'), colnames(dat))]
    error_twoworse_areworse = apply(sub_twoworse_dat, 1, function(row){
      row['treatmenttreatment1-treatmenttreatment3'] == '1' & 
        row['treatmenttreatment1-treatmenttreatment4'] == '1' & 
        row['treatmenttreatment2-treatmenttreatment3'] == '2' & 
        row['treatmenttreatment2-treatmenttreatment4'] == '2' & 
        row['treatmenttreatment3-treatmenttreatment4'] == 'overlap'  
    })
    
    errorD = errorC & error_secworse_isworst & !error_twoworse_areworse
    
    outD = data.frame(n = dat$n, 
                      method = dat$method, 
                      t2error = errorD, 
                      type = 'Either of bottom 2 pre-defined worst treatment identified as worst', 
                      scenario = Scenario)
    
    out_raw = rbind(outA, outB, outC, outD)
    
    # calculate power per sample size (n), per method, per type using power = (1 - mean(error))
    out = out_raw %>% 
      group_by(n, method, type) %>% 
      summarise(type2error = mean(t2error))
    out$power = 1 - out$type2error
    
  } else {
    message('Check scenario label. It should be input as integer.integer')
  }
  
  return(out)
}
