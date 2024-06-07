plot_best <- function (Scenario, plot.data, .font_size = font_size, .pt_size = pt_size,name.y) {
  plot.data <- plot.data$identifyBest
  method_type <- rep(NA,length(plot.data$method))
  method_type[grep('FE', plot.data$method)] <- "Fixed"
  method_type[grep('ME', plot.data$method)] <- "Mixed"
  plot.data <- data.frame(plot.data, method_type=as.factor(method_type))
  
  ggplot(plot.data, aes(x = n, y = power, linetype = method_type,shape = method, group = method)) +
      geom_point(size = .pt_size) +
      scale_shape_manual(values = shapes, name = '') +
      geom_line(linewidth=0.5, color = "#4d4d4d") + 
      guides(shape=guide_legend(ncol=2, byrow=TRUE), name = '') +
      labs(shape = NULL)+
      scale_x_continuous(breaks = unique(plot.data$n)) +labs(
        linetype = NULL,
        color = NULL,
        x = "Sample Size",
        y = name.y
      ) +
      scale_y_continuous(limits = c(0.5, 1), 
                         breaks = seq(0.5, 1, length.out = 11), 
                         labels = scaleFUN) +
      theme_minimal()+
      theme(
        legend.position = "bottom",
        legend.spacing.y = unit(0.02, 'cm'),
        legend.margin=margin(0,0,0,0),
        text = element_text(size = .font_size, color = "#4d4d4d"),
        legend.title = element_text(),
        legend.key.size = unit(0.5, 'cm'),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5))+ 
      guides(color = guide_legend(ncol = 2), 
             shape = guide_legend(ncol = 2))

}
