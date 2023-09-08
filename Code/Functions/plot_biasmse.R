# plot bias, coverage and MSE


plot_biasmse <- function(Scenario) {
  
  scenario <- unique(file_list)
  samplesize <- names(output[[1]])
  method <- names(output[[1]][[1]][["analyse_out"]][["method.property"]])
  metrics <- c("bias", "empirical_var","coverage_prob","mse","mortality_gain")
  treatment <- 1:nrow(output[[1]][[1]][["analyse_out"]][["method.property"]][[1]])
  
  scenario <- rep(scenario,each = length(samplesize)*length(method)*length(metrics)*length(treatment))
  samplesize <- rep(rep(samplesize, each = length(method)*length(metrics)*length(treatment)),length(unique(scenario)))
  method <- rep(rep(method, each = length(metrics)*length(treatment)),length(unique(scenario))*length(unique(samplesize)))
  metrics <- rep(rep(metrics, each = length(treatment)), length(unique(scenario))*length(unique(samplesize))*length(unique(method)))
  treatment <- rep(treatment, length(unique(scenario))*length(unique(samplesize))*length(unique(method))*length(unique(metrics)))
  result <- data.frame(scenario, samplesize, method, metrics, treatment)
  
  ## get the value for each combination 
  value <- sapply(1:nrow(result), function(i){
    scenario = result$scenario[i]
    samplesize = result$samplesize[i]
    method = result$method[i]
    treatment = result$treatment[i]
    metrics = result$metrics[i]
    if(metrics %in% c("bias", "empirical_var","coverage_prob","mse")) {
      return(as.numeric(output[[scenario]][[samplesize]][["analyse_out"]][["method.property"]][[method]][treatment,metrics]))
    }
    if(metrics %in% c("mortality_gain")){
      method_all <- sapply(row.names(output[[scenario]][[samplesize]][["analyse_out"]][["estimand2"]]),function(x){
        paste("est",x,sep = "_")
      })
      return(as.numeric(output[[scenario]][[samplesize]][["analyse_out"]][["estimand2"]][which(method_all==method),1]))
    }
  })
  
  result <- data.frame(result, value)
  
  result_plot <- result
  ### label the method using the same symbol as those used in simulations
  
  result_plot$method <- factor(substr(result_plot$method, 12, nchar(result_plot$method)))
  result_plot$scenario <- factor(substr(result_plot$scenario, 1, nchar(result_plot$scenario)-4))
  result_plot$samplesize <- as.numeric(substr(result_plot$samplesize, 17, nchar(result_plot$samplesize)))
  ## Levels: C C_NI C_str C_wk C2 C2_NI C2_str C2_wk
  ## label change to -> 1 1_NI 1_str 1_wk 2 2_NI 2_str 2_wk 
  result_plot$treatment <- factor(result_plot$treatment)
  result_plot$metrics <- factor(result_plot$metrics, levels = c("bias","empirical_var","coverage_prob","mse","mortality_gain"),
                                labels = c("Relative bias of treatment contrasts (%)","Empirical variance", "Coverage probability (%)", "Mean squared error (%)", "Mortality reduction (%)"))
  
  
  f1 = ggplot(subset(result_plot, scenario==Scenario&metrics!="Mortality reduction (%)"&metrics!="Empirical variance"), aes(x=samplesize, y=value, color=method,group=interaction(treatment,method), shape=method))+
    facet_wrap(metrics~., scales = "free_y",strip.position = "top", ncol = 1)+
    geom_point(size=1.4, position = position_dodge(width = 45))+ 
    guides(color=guide_legend(nrow=1, byrow=TRUE))+
    scale_shape_manual(values = shapes)+
    scale_color_manual(values = colors)+
    labs(
      shape = NULL, 
      color=NULL,
      title = "Properties of estimated treatment contrasts for scenarios xxx",
      subtitle = "Each point from left to right for each method represents a performance measure of an estimated \ncontrast between treatments 2, 3, and 4 relative to treatment 1.",
      x = "Sample Size",
      y = NULL,  # No y-axis label to avoid redundancy
    )+
    scale_x_continuous(breaks = c(100,150,200))+
    geom_segment(aes(x = 125, xend = 125, y = -Inf, yend = Inf), color = "red", linetype = "dashed") +
    geom_segment(aes(x = 175, xend = 175, y = -Inf, yend = Inf), color = "red", linetype = "dashed") +
    xlab("Sample size") +
    theme_minimal()+
    theme(
      plot.title.position = "plot",
      legend.position = "bottom",
      strip.text.x = element_text(size = 10),
      strip.background = element_blank(), 
      legend.spacing.y = unit(0.02, 'cm'),
      legend.margin=margin(0,0,0,0),
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 10),
      legend.text = element_text(size = 10),
      legend.title = element_text(),
      legend.key.size = unit(0.5, 'cm'),
      panel.spacing = unit(0.05, 'cm'),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5),
      panel.grid.major.x = element_blank()
    )
  ggsave(paste0(wd,"Plot/",Scenario,"_result_plot.png"),f1,width = 8,height = 7)
  
  return(f1)
}



lapply(1:length(unique(result_plot$scenario)), function(x){
  genFigure(Scenario = as.character(unique(result_plot$scenario)[x]),Metrics = "estimation")
})
lapply(1:length(unique(result_plot$scenario)), function(x){
  genFigure(Scenario = as.character(unique(result_plot$scenario)[x]),Metrics = "mortality")
})






