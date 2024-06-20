# plot type 1 error - for NULL scenarios only

plot_type1 <- function (plot.data, .font_size = font_size, .pt_size = pt_size,.metric,range.y,.label) {
  method_type <- rep(NA,length(plot.data$method))
  method_type[grep('FE', plot.data$method)] <- "Fixed"
  method_type[grep('ME', plot.data$method)] <- "Mixed"
  plot.data <- data.frame(plot.data, method_type=as.factor(method_type))
  plot.data <- subset(plot.data, type %in% .metric)
  plot.data$type <- factor(plot.data$type, levels = .metric, labels = .label)
  ggplot(plot.data, aes(x = n, y = type1error, shape = method, linetype = method_type,group = method)) +
    geom_point(size = .pt_size) +
    scale_shape_manual(values = shapes, name = '') +
    geom_line(linewidth=0.5)+ 
    labs(shape = NULL)+
    scale_x_continuous(breaks = unique(plot.data$n)) +
    labs(linetype = NULL,
         shape = NULL,
         x = "Sample Size",
         y = "Type 1 error"
    )  +
    facet_wrap(vars(type), nrow = 1) +
    theme_minimal()+
    theme(
      legend.position = "bottom",
      legend.spacing.y = unit(0.02, 'cm'),
      legend.margin=margin(0,0,0,0),
      text = element_text(size = .font_size, color = "#4d4d4d"),
      legend.title = element_text(),
      legend.key.size = unit(0.5, 'cm'),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5)) + 
    guides(color = guide_legend(ncol = 2), 
           shape = guide_legend(ncol = 2))+
    coord_cartesian(ylim =  range.y) 
    
  
}
