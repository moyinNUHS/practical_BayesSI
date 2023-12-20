plot_timing <- function(Scenario, Timing){
  plot.data <- data.frame(scenario=Scenario, timing = Timing)
  
  ggplot(plot.data, aes(x = Scenario, y = Timing, group=1)) +
    geom_point()+
    geom_line(linetype = 2,linewidth=0.5)+ 
    labs(
      x = "Scenario",
      y = "Running time (min)",
      subtitle = "The running time for 100 iterations"
    ) +
    theme_minimal()+
    theme(
      text = element_text(size = font_size, color = "#4d4d4d")) 
}
