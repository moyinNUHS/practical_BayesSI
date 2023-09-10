# plot type 1 error - for NULL scenarios only

plot_type1 <- function (Scenario, d, .method_labs = method_labs) {
  
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
      df_list = iter[grep('method_', names(iter))]
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
  
  dat.lean = dat[, grep('treatment', names(dat))]
  
  ################## 
  # calculate type 1 error 
  ##################
  
  # here, type 1 error - 
  # (A) Identified 1 best treatment (terminating trial for efficacy)
  # (B) Identified 2 better treatments 
  # (C) Identified 3 better treatments 
  # (D) Identified any 1 treatment as better than the other but 1 contiguous group 
  # (E) All of the above errors 
  
  # (A) Identified 1 best treatment (terminating trial for efficacy)
  errorA = apply(dat.lean, 1, function(row){
    row = row[-which(row == 'overlap')]
    any(table(as.character(row)) == n_tx)
  })
  
  outA = data.frame(n = dat$n, 
                    method = dat$method, 
                    t1error = errorA, 
                    type = 'Identified 1 best treatment (terminating trial for efficacy)',
                    scenario = Scenario)
  
  # (B) Identified 2 better treatments 
  errorB = apply(dat.lean, 1, function(row){
    sum(row == 'overlap') == 2
  })
  
  outB = data.frame(n = dat$n, 
                    method = dat$method, 
                    t1error = errorB, 
                    type = 'Identified 2 better treatments (dropping 2 treatments)',
                    scenario = Scenario)
  
  # (C) Identified 3 better treatments 
  errorC = apply(dat.lean, 1, function(row){
    row = row[-which(row == 'overlap')]
    length(unique(row)) == n_tx
  })
  
  outC = data.frame(n = dat$n, 
                    method = dat$method, 
                    t1error = errorC, 
                    type = 'Identified 3 better treatments (dropping a treatment) ',
                    scenario = Scenario)
  
  # (D) Identified any 1 treatment as better than the other but in 1 contiguous group
  errorD = apply(dat.lean, 1, function(row){
    sum(row != 'overlap') == 1
  })
  
  outD = data.frame(n = dat$n, 
                    method = dat$method, 
                    t1error = errorD, 
                    type = 'Identified any 1 as better than any other treatment but in 1 contiguous group',
                    scenario = Scenario)
  
  # (E) Any of the above errors 
  errorE = apply(dat.lean, 1, function(row){
    any(row != 'overlap')
  })
  
  outE = data.frame(n = dat$n, 
                    method = dat$method, 
                    t1error = errorE, 
                    type = 'Any treament different from other treatment(s)',
                    scenario = Scenario)
  
  # (F) 1 or more better than the other treatment(s)
  errorF = errorA | errorB | errorC
  
  outF = data.frame(n = dat$n, 
                    method = dat$method, 
                    t1error = errorF, 
                    type = '1 or more better than the other treatment(s)',
                    scenario = Scenario)
  
  # calculate type 1 error per sample size (n), per method, per type using power = (1 - mean(error))
  out_raw = rbind(outA, outB, outC, outD, outE, outF)
  
  out = out_raw %>% 
    group_by(n, method, type) %>% 
    summarise(type1error = mean(t1error))
  
  out$type = factor(out$type, levels = c('Identified 1 best treatment (terminating trial for efficacy)',
                                         'Identified 2 better treatments (dropping 2 treatments)',
                                         'Identified 3 better treatments (dropping a treatment) ',
                                         'Identified any 1 as better than any other treatment but in 1 contiguous group',
                                         '1 or more better than the other treatment(s)', 
                                         'Any treament different from other treatment(s)'))
  
  ################## 
  # plot 
  ################## 
  
  ggplot(out, aes(x = n, y = type1error, color = method, group = method)) +
    geom_point(shape=3)+
    geom_line(linetype = 2,linewidth=0.5)+ 
    guides(color=guide_legend(nrow=2, byrow=TRUE))+
    scale_color_manual(values = colors)+
    labs(shape = NULL, color=NULL)+
    scale_x_continuous(breaks = unique(out$n)) +labs(
      linetype = NULL,
      color = NULL,
      x = "Sample Size",
      y = "Type 1 error"
    ) +
    scale_y_continuous(limits = c(0, 1), 
                       breaks = seq(0, 1, length.out = 11), 
                       labels = scaleFUN) +
    facet_wrap(vars(type), nrow = 3) +
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
  
}