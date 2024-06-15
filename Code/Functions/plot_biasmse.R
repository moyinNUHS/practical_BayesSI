# plot bias, coverage and MSE


plot_biasmse <- function(Scenario, .method_labs = method_labs, d, .all_method_names = all_method_names, 
                         .tx_labs = tx_labs,
                         .font_size = font_size, .pt_size = pt_size,.metric, name.y, range.y) {
  
  # make a long form data
  n = parse_number(names(d))
  wide_raw = list()
  for (i in n) {
    subset_size = d[[grep(paste0(paste("=",as.character(i),sep=" "),"$"), names(d))]]
    raw = as.data.frame(do.call(rbind, subset_size$analyse_out$method.property))
    raw$method = rep(names(subset_size$analyse_out$method.property), each = nrow(subset_size$analyse_out$method.property[[1]]))
    raw$n = i
    raw$treatment = paste0('treatment', substr(rownames(raw), 4, 4))
    wide_raw[[i]] = raw
  }
  wide_wide = as.data.frame(do.call(rbind, wide_raw))
  wide = wide_wide[,which(colnames(wide_wide) %in% c("n", "treatment", "method", "bias", 
                                                     #"empirical_var",
                                                     "coverage_prob","mse"))]
  wide$method = factor(wide$method, 
                       levels = all_method_names, 
                       labels = .method_labs)
  
  long = reshape2::melt(wide, id.var = c('method', 'treatment', 'n'), 
                        variable.name = 'metric')
  long$metric = factor(long$metric, levels = c("bias",
                                               #"empirical_var",
                                               "coverage_prob","mse"),
                       labels = c("Bias of treatment contrasts (%)",
                                  # "Empirical variance", 
                                  "Coverage probability (%)", 
                                  "Mean squared error (%)"))
  
  # plot 
  ##gaps = diff(unique(long$n))[1]
  #br_x = seq(min(long$n) + gaps/2, max(long$n) - gaps/2, length.out = length(unique(long$n)) - 1)
  br_x <- (head(unique(long$n), -1) + tail(unique(long$n), -1)) / 2
  long_temp <- subset(long, metric==.metric)
  f = ggplot(long_temp, aes(x = n, y = value, color = treatment, 
                       group = interaction(treatment, method), 
                       shape = method)) +
    #facet_wrap(metric ~., scales = "free_y",strip.position = "top", ncol = 1) +
    geom_point(size = .pt_size, position = position_dodge(width = 500))+ 
    scale_shape_manual(values = shapes) +
    scale_color_manual(values = colors) +
    labs(
      shape = NULL, 
      color=NULL,
      x = "Sample Size",
      y =  name.y  # No y-axis label to avoid redundancy
    ) +
    scale_x_continuous(breaks = unique(long$n)) +
    geom_vline(xintercept = br_x, color = "black", linetype = "dashed") +
    xlab("Sample size") +
    theme_minimal()+
    theme(
      plot.title.position = "plot",
      legend.position = "bottom",
      strip.background = element_blank(), 
      legend.spacing.y = unit(0.02, 'cm'),
      legend.margin=margin(0,0,0,0),
      text = element_text(size = font_size),
      legend.title = element_text(),
      legend.key.size = unit(0.5, 'cm'),
      panel.spacing = unit(0.05, 'cm'),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5),
      panel.grid.major.x = element_blank()
    )+ 
    guides(color = guide_legend(ncol = 2), 
           shape = guide_legend(ncol = 2))+
    coord_cartesian(ylim =  range.y) 

  
  return(f)
}






