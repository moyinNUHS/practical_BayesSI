# plot type 1 error - for NULL scenarios only

plot_type1 <- function (plot.data, .font_size = font_size) {
  
  ggplot(plot.data, aes(x = n, y = type1error, color = method, group = method)) +
    geom_point(shape=3)+
    geom_line(linetype = 2,linewidth=0.5)+ 
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
      text = element_text(size = font_size, color = "#4d4d4d"),
      legend.title = element_text(),
      legend.key.size = unit(0.5, 'cm'),
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5)) + 
    guides(color = guide_legend(ncol = 2), 
           shape = guide_legend(ncol = 2))
  
}