# plot mortality reduction

plot_mort <- function(Scenario, d, .method_labs = method_labs, .all_method_names = all_method_names, 
                      .tx_labs = tx_labs,
                      .font_size = font_size, .pt_size = pt_size,.metric, name.y,range.y ) {
  
  # make a long form data
  n = parse_number(names(d))
  wide_raw = list()
  for (i in n) {
    subset_size = d[[grep(paste0(paste("=",as.character(i),sep=" "),"$"), names(d))]]
    raw = as.data.frame(subset_size$analyse_out$estimand2)
    raw$method = rownames(raw)
    raw$n = i
    wide_raw[[i]] = raw
  }
  wide = as.data.frame(do.call(rbind, wide_raw))
  # long = wide[,which(colnames(wide) %in% c("n", "treatment", "method", "mortality_gain"))]
  mort_all_method_names = substr(.all_method_names, 5, nchar(.all_method_names))
  wide$method = factor(wide$method, 
                       levels = mort_all_method_names, 
                       labels = .method_labs)
  long = reshape2::melt(wide, id.var = c('method', 'n'))
   method_type <- rep(NA,length(long$method))
  method_type[grep('FE', long$method)] <- "Fixed"
  method_type[grep('ME', long$method)] <- "Mixed"
  long <- data.frame(long, method_type=as.factor(method_type))
  long_temp <- subset(long, variable==.metric)
  # plot 
  f = ggplot(long_temp, aes(x = n, y = value, shape = method, linetype = method_type,group = method)) +
    geom_point(size = .pt_size) +
    scale_shape_manual(values = shapes, name = '') +
    geom_line(linewidth=0.5, color = "#4d4d4d") +
    guides(shape=guide_legend(ncol=1, byrow=TRUE), name = '') +
    scale_x_continuous(breaks = unique(long$n))+
    labs(
      linetype = NULL,
      shapes = NULL,
      x = "Sample Size",
      y = name.y
    ) +
    theme_minimal() +
    #facet_wrap(variable ~., scales = "free", ncol = 1) + 
    theme(
      plot.title.position = "plot",
      legend.position = "bottom",
      legend.spacing.y = unit(0.02, 'cm'),
      legend.margin=margin(0,0,0,0),
      text = element_text(size = .font_size, color = "#4d4d4d"),
      legend.title = element_text(),
      legend.key.size = unit(0.5, 'cm'),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5))+
    coord_cartesian(ylim = range.y) + 
    guides(color = guide_legend(ncol = 2), 
           shape = guide_legend(ncol = 2))
  
  return(f)
  
}







