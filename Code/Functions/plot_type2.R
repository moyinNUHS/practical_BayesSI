# plot type 2 error - for non-NULL scenarios only

plot_type2 <- function (Scenario, d, .method_labs = method_labs) {
  
  # number of treatment comparisons
  n_tx = nrow(d[[1]][['scenario_out']][[1]][['est_method_1']]) - 1
  
  ##################
  # collate data for contiguous groups 
  ##################
  n = parse_number(names(d))
  contig = list()
  
  for (i in n) { # for each sample size 
    
    # subset data for the sample size 
    subset_n = d[[grep(as.character(i), names(d))]]$scenario_out
    
    # for each iteration, apply find_contig_grp function
    iter_conti = lapply(subset_n, function(iter){
      
      # list of all data.frames with estimates, each refers to a method
      df_list = iter[grep('est_method', names(iter))]
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
                      levels = c("est_method_1", "est_method_1_NI", "est_method_1_wk", "est_method_1_str",
                                 "est_method_2", "est_method_2_NI", "est_method_2_wk","est_method_2_str"), 
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
                         type = 'Pre-defined best treatment not identified as best',
                         scenario = Scenario)
    
    # calculate power per sample size (n), per method, per type using power = (1 - mean(error))
    out = out_raw %>% 
      group_by(n, method, type) %>% 
      summarise(type2error = mean(t2error))
    out$power = 1 - out$type2error
    
    ################## 
    # plot 
    ################## 
    ggplot(out, aes(x = n, y = power, color = method, group = method)) +
      geom_point(shape=3)+
      geom_line(linetype = 2,linewidth=0.5)+ 
      guides(color=guide_legend(nrow=2, byrow=TRUE))+
      scale_color_manual(values = colors)+
      labs(shape = NULL, color=NULL)+
      scale_x_continuous(breaks = unique(out$n)) +labs(
        linetype = NULL,
        color = NULL,
        x = "Sample Size",
        y = "Power"
      ) +
      scale_y_continuous(limits = c(0, 1), 
                         breaks = seq(0, 1, length.out = 11), 
                         labels = scaleFUN) +
      theme_minimal()+
      theme(
        legend.position = "bottom",
        legend.spacing.y = unit(0.02, 'cm'),
        legend.margin=margin(0,0,0,0),
        text = element_text(size = 14, color = "#4d4d4d"),
        legend.title = element_text(),
        legend.key.size = unit(0.5, 'cm'),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5))
    
    
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
      sum(row != best_tx) != 0 # error is committed when any of the comparisons do not show best_tx as the best 
    })
    
    outA = data.frame(n = dat$n, 
                      method = dat$method, 
                      t2error = errorA, 
                      type = 'Pre-defined best treatment not identified as best',
                      scenario = Scenario)
    
    # (B) when treatment 1 or 2 were not the best treatments
    sub_secbest_dat = dat[, grep(paste0('treatment', secbest_tx), colnames(dat))]
    errorB = apply(sub_secbest_dat, 1, function(row){
      sum(row != secbest_tx) != 0 
    })
    
    error = errorA & errorB # error is committed when either top 2 predefined treatments were not concluded as the best 
    
    outB = data.frame(n = dat$n, 
                      method = dat$method, 
                      t2error = error, 
                      type = 'Either of top 2 pre-defined best treatment not identified as best', 
                      scenario = Scenario)
    
    # (C) when treatment 4 was not the worst treatment 
    sub_worst_dat = dat[, grep(paste0('treatment', worst_tx), colnames(dat))]
    errorC = apply(sub_worst_dat, 1, function(row){
      sum(row %in% c(best_tx, secbest_tx, secworse_tx)) != n_tx 
    })
    
    outC = data.frame(n = dat$n, 
                      method = dat$method, 
                      t2error = errorC, 
                      type = 'Pre-defined worst treatment not identified as worst', 
                      scenario = Scenario)
    
    # (D) when treatment 3 or 4 were not the worst treatments 
    sub_secworse_dat = dat[, grep(paste0('treatment', secworse_tx), colnames(dat))]
    errorD = apply(sub_secworse_dat, 1, function(row){
      sum(row %in% c(best_tx, secbest_tx, secworse_tx)) != n_tx 
    })
    
    error = errorC & errorD
    
    outD = data.frame(n = dat$n, 
                      method = dat$method, 
                      t2error = error, 
                      type = 'Either of bottom 2 pre-defined worst treatment not identified as worst', 
                      scenario = Scenario)
    
    out_raw = rbind(outA, outB, outC, outD)
    
    # calculate power per sample size (n), per method, per type using power = (1 - mean(error))
    out = out_raw %>% 
      group_by(n, method, type) %>% 
      summarise(type2error = mean(t2error))
    out$power = 1 - out$type2error
    
    ################## 
    # plot 
    ################## 
    ggplot(out, aes(x = n, y = power, color = method, group = method)) +
      geom_point(shape=3)+
      geom_line(linetype = 2,linewidth=0.5)+ 
      guides(color=guide_legend(nrow=2, byrow=TRUE))+
      scale_color_manual(values = colors)+
      labs(shape = NULL, color=NULL)+
      scale_x_continuous(breaks = unique(out$n)) +labs(
        linetype = NULL,
        color = NULL,
        x = "Sample Size",
        y = "Power"
      ) +
      scale_y_continuous(limits = c(0, 1), 
                         breaks = seq(0, 1, length.out = 11), 
                         labels = scaleFUN) +
      facet_wrap(vars(type)) +
      theme_minimal()+
      theme(
        legend.position = "bottom",
        legend.spacing.y = unit(0.02, 'cm'),
        legend.margin=margin(0,0,0,0),
        text = element_text(size = 14, color = "#4d4d4d"),
        legend.title = element_text(),
        legend.key.size = unit(0.5, 'cm'),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5))
    
  } else {
    message('Check scenario label. It should be input as integer.integer')
  }
  
}
