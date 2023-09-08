# plot mortality reduction

plot_mort <- function(Scenario, d, method_labs) {
  
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
  wide = long_wide[,which(colnames(wide_wide) %in% c("treatment", "method", "bias", "empirical_var","coverage_prob","mse"))]
  wide$method = factor(wide$method, 
                       levels = c("est_method_1", "est_method_1_NI", "est_method_1_wk", "est_method_1_str",
                                  "est_method_2", "est_method_2_NI", "est_method_2_wk","est_method_2_str"), 
                       labels = method_labs)
  
  long = reshape2::melt(wide, id.var = c('method', 'treatment'), 
                        variable.name = 'metric')
  long$metric <- factor(long$metric, levels = c("bias","empirical_var","coverage_prob","mse"),
                         labels = c("Relative bias of treatment contrasts (%)",
                                    "Empirical variance", 
                                    "Coverage probability (%)", 
                                    "Mean squared error (%)"))
  
  # plot 
  
  f = ggplot(subset(result_plot, scenario==Scenario&metrics=="Mortality reduction (%)"), aes(x=samplesize, y=value, color=method, group=method))+
    geom_point(shape=3)+
    geom_line(linetype = 2,linewidth=0.5)+ 
    guides(color=guide_legend(nrow=2, byrow=TRUE))+
    scale_color_manual(values = colors)+
    labs(shape = NULL, color=NULL)+
    scale_x_continuous(breaks = c(100,150,200))+
    labs(
      linetype = NULL,
      color = NULL,
      title = "Mortality Reduction by Sample Size",
      x = "Sample Size",
      y = "Mortality reduction (%)"
    )+
    theme_minimal()+
    theme(
      plot.title.position = "plot",
      plot.title = element_text(size = 17, face = "bold", color = "#4d4d4d", margin = margin(b = 10)),
      plot.subtitle = element_text(size = 14, color = "#4d4d4d", margin = margin(b = 20)),
      legend.position = "bottom",
      legend.spacing.y = unit(0.02, 'cm'),
      legend.margin=margin(0,0,0,0),
      axis.text = element_text(size = 12, color = "#4d4d4d"),
      axis.title = element_text(size = 14, color = "#4d4d4d"),
      legend.text = element_text(size = 12, color = "#4d4d4d"),
      legend.title = element_text(),
      legend.key.size = unit(0.5, 'cm'),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5))
  
  ggsave(paste0(wd,"Plots/", Scenario,"_mortality_plot.png"), f1,width = 8,height = 5)
  
  return(f1)
  
}

lapply(1:length(unique(result_plot$scenario)), function(x){
  genFigure(Scenario = as.character(unique(result_plot$scenario)[x]),Metrics = "estimation")
})
lapply(1:length(unique(result_plot$scenario)), function(x){
  genFigure(Scenario = as.character(unique(result_plot$scenario)[x]),Metrics = "mortality")
})






