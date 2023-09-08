# plot mortality reduction

plot_mort <- function(Scenario) {
  
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
  
  f1 = ggplot(subset(result_plot, scenario==Scenario&metrics=="Mortality reduction (%)"), aes(x=samplesize, y=value, color=method, group=method))+
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






