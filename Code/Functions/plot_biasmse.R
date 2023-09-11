# plot bias, coverage and MSE


plot_biasmse <- function(Scenario, .method_labs = method_labs, d) {
  
  # make a long form data
  n = parse_number(names(d))
  wide_raw = list()
  for (i in n) {
    subset_size = d[[grep(as.character(i), names(d))]]
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
                       levels = c("est_method_1", "est_method_1_NI", "est_method_1_wk", "est_method_1_str",
                                  "est_method_2", "est_method_2_NI", "est_method_2_wk","est_method_2_str"), 
                       labels = .method_labs)
  
  long = reshape2::melt(wide, id.var = c('method', 'treatment', 'n'), 
                        variable.name = 'metric')
  long$metric = factor(long$metric, levels = c("bias",
                                               #"empirical_var",
                                               "coverage_prob","mse"),
                       labels = c("Relative bias of treatment contrasts (%)",
                                  # "Empirical variance", 
                                  "Coverage probability (%)", 
                                  "Mean squared error (%)"))
  
  # plot 
  gaps = diff(unique(long$n))[1]
  br_x = seq(min(long$n) + gaps/2, max(long$n) - gaps/2, length.out = length(unique(long$n)) - 1)
  
  f = ggplot(long, aes(x = n, y = value, color = method, 
                       group = interaction(treatment, method), 
                       shape = method)) +
    facet_wrap(metric ~., scales = "free_y",strip.position = "top", ncol = 1) +
    geom_point(size=1.4, position = position_dodge(width = 45))+ 
    guides(color = guide_legend(nrow=1, byrow=TRUE))+
    scale_shape_manual(values = shapes) +
    scale_color_manual(values = colors) +
    labs(
      shape = NULL, 
      color=NULL,
      x = "Sample Size",
      y = NULL,  # No y-axis label to avoid redundancy
    ) +
    scale_x_continuous(breaks = unique(long$n)) +
    geom_vline(xintercept = br_x, color = "red", linetype = "dashed") +
    xlab("Sample size") +
    theme_minimal()+
    theme(
      plot.title.position = "plot",
      legend.position = "bottom",
      strip.background = element_blank(), 
      legend.spacing.y = unit(0.02, 'cm'),
      legend.margin=margin(0,0,0,0),
      text = element_text(size = 15),
      legend.title = element_text(),
      legend.key.size = unit(0.5, 'cm'),
      panel.spacing = unit(0.05, 'cm'),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5),
      panel.grid.major.x = element_blank()
    )

  
  return(f)
}






