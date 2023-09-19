# plot type 2 error - for non-NULL scenarios only

plot_type2 <- function (Scenario, plot.data, .font_size = font_size) {
  
  
  if (Scenario == '1.2') {
    
    ggplot(plot.data, aes(x = n, y = power, color = method, group = method)) +
      geom_point(shape=3)+
      geom_line(linetype = 2,linewidth=0.5)+ 
      scale_color_manual(values = colors)+
      labs(shape = NULL, color=NULL)+
      scale_x_continuous(breaks = unique(plot.data$n)) +labs(
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
        panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5))+ 
      guides(color = guide_legend(ncol = 2), 
             shape = guide_legend(ncol = 2))
    
    
  } else if (Scenario %in% c('1.3', '1.4', '2.2', '2.4', '3.2', '4.1', '4.2', '4.3')){
    
    
    ggplot(plot.data, aes(x = n, y = power, color = method, group = method)) +
      geom_point(shape=3)+
      geom_line(linetype = 2,linewidth=0.5)+ 
      scale_color_manual(values = colors)+
      labs(shape = NULL, color=NULL)+
      scale_x_continuous(breaks = unique(plot.data$n)) +labs(
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
        text = element_text(size = font_size, color = "#4d4d4d"),
        legend.title = element_text(),
        legend.key.size = unit(0.5, 'cm'),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5))+ 
      guides(color = guide_legend(ncol = 2), 
             shape = guide_legend(ncol = 2))
    
  } else {
    message('Check scenario label. It should be input as integer.integer')
  }
  
}
