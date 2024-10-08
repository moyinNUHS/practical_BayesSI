ex_performance_modified <- function(q, k, output_replication, R) {
  mat_all <- do.call(cbind, lapply(1:R, function(z) {
    output_replication[[z]]$performance_m[[q]][, k]
  }))
  apply(mat_all, 1, function(x)
    mean(x==0, na.rm = T))
}

get_type2 <- function(Scenario, d, .method_labs = method_labs, .all_method_names = all_method_names, no_pattern=4) {
  
  
  # number of treatment comparisons
  n_tx = nrow(d[[1]][['scenario_out']][[1]][['est_method_1']]) - 1
  
  ##################
  # collate data for contiguous groups 
  ##################
  n = parse_number(names(d))
  contig = list()
  
  for (i in n) { # for each sample size 
    
    # subset data for the sample size 
    subset_n = d[[grep(paste0(paste("=",as.character(i),sep=" "),"$"), names(d))]]$scenario_out
    
    # for each iteration, apply find_contig_grp function
    iter_conti = lapply(subset_n, function(iter){
      
      # list of all data.frames with estimates, each refers to a method
      df_list = iter[grep('est_method', names(iter))]
      df_list = lapply(df_list, function(x){rownames(x)=c("treatment1","treatment2","treatment3","treatment4"); return(x)})
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
    
  } 
  else if(Scenario == '1.4'){
    worst_tx = '2'
    
    # here, type 2 error - 
    # when treatment 2 was not the worst treatment 
    # hence, type 2 error condition : treatment1-treatment2 = 2, or 
    #                                 treatment2-treatment3 = 2, or  
    #                                 treatment2-treatment4 = 2 
    
    sub_worst_dat = dat[, grep(paste0('treatment', worst_tx), colnames(dat))]
    
    errorA = apply(sub_worst_dat, 1, function(row){
      #sum(row != worst_tx) != n_tx  ##this allows for overlap so I commented out 
      
      row['treatment1-treatment2'] != '1' |
        row['treatment2-treatment3'] != '3' |
        row['treatment2-treatment4'] != '4'   ##error is committed if worst_tx is not clearly worst (no overlap) than all others
      
    })

    outA = data.frame(n = dat$n, 
                         method = dat$method, 
                         t2error = errorA, 
                         type = 'Pre-defined worst treatment identified as worst',
                         scenario = Scenario)
    
    
  # errorB = apply(sub_worst_dat, 1, function(row){
    
   #   row['treatment1-treatment2'] != '1' &
   #     row['treatment2-treatment3'] != '3' &
    #    row['treatment2-treatment4'] != '4'  ##error is committed only if NONE of three best is clearly better (no overlap) than predefined worst
        
   # })
    
   # outB = data.frame(n = dat$n, 
    #                  method = dat$method, 
     #                 t2error = errorB, 
      #                type = '1 or more best treatments identified as better than worst',
      #                scenario = Scenario)
      
    out_raw = outA
    
    # calculate power per sample size (n), per method, per type using power = (1 - mean(error))
    out = out_raw %>% 
      group_by(n, method, type) %>% 
      summarise(type2error = mean(t2error))
    out$power = 1 - out$type2error
  }
  else if (Scenario %in% c('1.3', '1.5', '1.6' ,'2.2', '2.3', '3.2', '3.4', '4.2')){
    if(Scenario == '1.6'){
      best_tx = '2'
      secbest_tx = '1'
      worst_tx = '3'
      secworse_tx = '4'
    }
    else{
      best_tx = '1'
      secbest_tx = '2'
      worst_tx = '4'
      secworse_tx = '3'
    }
    # here, type 2 error - 
    # Failure to identify best treatments (terminating trial for efficacy)
    # (A) when best_tx was not the best treatment 
    # (B) when best_tx or secbest_tx were not the best treatments
    # Failure to identify worst treatments (drop treatment for safety)
    # (C) when worst_tx was not the worst treatment 
    # (D) when worst_tx or secworse_tx were not the worst treatments 
    
    
    # (A) when best_tx was not the best treatment 
    sub_best_dat = dat[, grep(paste0('treatment', best_tx), colnames(dat))]
    errorA = apply(sub_best_dat, 1, function(row){
      any(row != best_tx) # error is committed when any of the comparisons do not show best_tx as the best 
    })
    
    outA = data.frame(n = dat$n, 
                      method = dat$method, 
                      t2error = errorA, 
                      type = 'Pre-defined best treatment identified as best',
                      scenario = Scenario)
    
    # (B) when best_tx or secbest_tx were not the best treatments
    sub_secbest_dat = dat[, grep(paste0('treatment', secbest_tx), colnames(dat))]
    error_secbest = apply(sub_secbest_dat, 1, function(row){
      any(row != secbest_tx) # error is committed when any of the comparisons do not show second best_tx as the best 
    })
    
    sub_twobest_dat = dat[, grep(paste0('treatment', c(secbest_tx, best_tx), collapse = '|'), colnames(dat))]
    error_twobest = apply(sub_twobest_dat, 1, function(row){
      row['treatment1-treatment2'] == 'overlap' & 
        row['treatment1-treatment3'] == '1' & 
        row['treatment1-treatment4'] == '1' & 
        row['treatment2-treatment3'] == '2' & 
        row['treatment2-treatment4'] == '2' 
    }) # error is committed when either top 2 predefined treatments were not concluded as the best 
    
    errorB = errorA & error_secbest & !error_twobest
    
    outB = data.frame(n = dat$n, 
                      method = dat$method, 
                      t2error = errorB, 
                      type = 'Either of top 2 pre-defined best treatment identified as best', 
                      scenario = Scenario)
    
    # (C) when worst_tx was not the worst treatment 
    sub_worst_dat = dat[, grep(paste0('treatment', worst_tx), colnames(dat))]
    errorC = apply(sub_worst_dat, 1, function(row){
      sum(row %in% c(best_tx, secbest_tx, secworse_tx)) != n_tx 
    })
    
    outC = data.frame(n = dat$n, 
                      method = dat$method, 
                      t2error = errorC, 
                      type = 'Pre-defined worst treatment identified as worst', 
                      scenario = Scenario)
    
    # (D) when worst_tx or secworse_tx were not the worst treatments 
    sub_secworse_dat = dat[, grep(paste0('treatment', secworse_tx), colnames(dat))]
    error_secworse_isworst = apply(sub_secworse_dat, 1, function(row){
      sum(row %in% c(best_tx, secbest_tx, worst_tx)) != n_tx 
    })
    
    sub_twoworse_dat = dat[, grep(paste0('treatment', c(worst_tx, secworse_tx), collapse = '|'), colnames(dat))]
    error_twoworse_areworse = apply(sub_twoworse_dat, 1, function(row){
      row['treatment1-treatment3'] == '1' & 
        row['treatment1-treatment4'] == '1' & 
        row['treatment2-treatment3'] == '2' & 
        row['treatment2-treatment4'] == '2' & 
        row['treatment3-treatment4'] == 'overlap'  
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
    
    } 
    else {
    message('Check scenario label. It should be input as integer.integer')
  }
  
  bestT <- list()
  
  for (i in n) { # for each sample size 
    
    # subset data for the sample size 
    output_replication = d[[grep(paste0(paste("=",as.character(i),sep=" "),"$"), names(d))]]$scenario_out
    R = length(output_replication)
    # for each iteration, apply find_contig_grp function
    ex_performance_out <- sapply(1:no_pattern, function(i)
      ex_performance_modified(q="diff_min", k=i, output_replication=output_replication, R=R))
    if(Scenario != '4.2'){
      lambda = c(P1 = 0.25, P2 = 0.25, P3 = 0.25, P4 = 0.25)
    }
    if(Scenario == '4.2'){
      lambda = c(P1 = 0.1, P2 = 0.3, P3 = 0.3, P4 = 0.3)
    }
    
    bestT[[i]] <- apply(ex_performance_out, 1, function(y) {
      sum(y * lambda) # multiplying performance measures for each method by pattern frequency 
    })
  }
  dat_best = do.call(rbind, bestT)
  dat_best = cbind(dat_best,n=n)
  colnames(dat_best) =  c(.all_method_names,"n")
  dat_best = as.data.frame(dat_best) %>% 
    pivot_longer(cols = 1:(ncol(dat_best)-1), names_to = "method", values_to = "power")
  dat_best$method = factor(dat_best$method, 
                           levels = .all_method_names, 
                           labels = .method_labs)
  return(list(out=out,identifyBest=dat_best))
}
