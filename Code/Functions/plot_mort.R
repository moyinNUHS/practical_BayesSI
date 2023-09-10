# plot mortality reduction

plot_mort <- function(Scenario, d, method_labs) {
  
  # make a long form data
  n = parse_number(names(d))
  wide_raw = list()
  for (i in n) {
    subset_size = d[[grep(as.character(i), names(d))]]
    raw = as.data.frame(subset_size$analyse_out$estimand2)
    raw$method = rownames(raw)
    raw$n = i
    wide_raw[[i]] = raw
  }
  wide = as.data.frame(do.call(rbind, wide_raw))
  long = wide[,which(colnames(wide) %in% c("n", "treatment", "method", "mortality_gain"))]
  long$method = factor(long$method, 
                       levels = c("method_1", "method_1_NI", "method_1_wk", "method_1_str",
                                  "method_2", "method_2_NI", "method_2_wk","method_2_str"), 
                       labels = method_labs)
  
  # plot 
  f = ggplot(long, aes(x = n, y = mortality_gain, color = method, group = method)) +
    geom_point(shape=3)+
    geom_line(linetype = 2,linewidth=0.5)+ 
    guides(color=guide_legend(nrow=2, byrow=TRUE))+
    scale_color_manual(values = colors)+
    labs(shape = NULL, color=NULL)+
    scale_x_continuous(breaks = unique(long$n))+
    labs(
      linetype = NULL,
      color = NULL,
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
  
  ggsave(paste0(wd,"Plots/", Scenario,"_mortality_plot.png"), f,width = 8, height = 6)
  
  return(f)
  
}







