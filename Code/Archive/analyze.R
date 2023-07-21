library(ggplot2)
#### set working directory
wd <- '/Users/cheryl/Documents/duke-nus/bibhas/practical/practical/'
setwd(wd)
### detect scenarios in the run_output

file_list <- list.files("Code/Run_output")
file_num <- length(list.files("Code/Run_output"))
output <-   lapply(1:file_num, function(i){
  readRDS(paste0(wd,"Code/Run_output/",file_list[i]))})

names(output) <- file_list

## get all settings with different scenarios, sample size, method, metrics, and treatment

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

result_plot$treatment <- factor(result_plot$treatment)
result_plot$metrics <- factor(result_plot$metrics, levels = c("bias","empirical_var","coverage_prob","mse","mortality_gain"),
                              labels = c("Relative bias of treatment contrasts (%)","Empirical variance", "Coverage probability (%)", "Mean squared error (%)", "Mortality reduction (%)"))

shapes <- c("C" = 0, "C_NI" = 1, "C_wk" = 2, "C_str" = 5,
            "C2" = 15, "C2_NI" = 16, "C2_wk" = 17, "C2_str" = 18)

colors <- c("C" = "#1f78b4", "C_NI" = "#33a02c", "C_wk" = "#e31a1c", "C_str" = "#ff7f00",
            "C2" = "#6a3d9a", "C2_NI" = "#a6cee3", "C2_wk" = "#b2df8a", "C2_str" = "#fb9a99")

ggplot(subset(result_plot, scenario=="scenario1.1"&metrics!="Mortality reduction (%)"&metrics!="Empirical variance"), aes(x=samplesize, y=value, color=method,group=interaction(treatment,method), shape=method))+
  facet_wrap(metrics~., scales = "free_y",strip.position = "top", ncol = 1)+
  geom_point(size=1.6, position = position_dodge(width = 40))+ 
  guides(color=guide_legend(nrow=1, byrow=TRUE))+
  scale_shape_manual(values = shapes)+
  scale_color_manual(values = colors)+
  labs(shape = NULL, color=NULL)+
  scale_x_continuous(breaks = c(100,150,200))+
  geom_segment(aes(x = 125, xend = 125, y = -Inf, yend = Inf), color = "red", linetype = "dashed") +
  geom_segment(aes(x = 175, xend = 175, y = -Inf, yend = Inf), color = "red", linetype = "dashed") +
  xlab("Sample size") +
  theme_bw()+
  theme(legend.position = "bottom",
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
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =0.5))


ggsave(paste0(wd,"Code/Archive/plot/","result_plot1.png"),width = 8,height = 7)

## plot mortality reduction
ggplot(subset(result_plot, scenario=="scenario1.1"&metrics=="Mortality reduction (%)"), aes(x=samplesize, y=value, color=method,group=method))+
  geom_line(linewidth=0.5)+ 
  guides(color=guide_legend(nrow=2, byrow=TRUE))+
  scale_shape_manual(values = shapes)+
  scale_color_manual(values = colors)+
  labs(shape = NULL, color=NULL)+
  scale_x_continuous(breaks = c(100,150,200))+
  xlab("Sample size") +
  ylab("Mortality reduction (%)")+
  theme_bw()+
  theme(legend.position = "bottom",
        legend.spacing.y = unit(0.02, 'cm'),
        legend.margin=margin(0,0,0,0),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.title = element_text(),
        legend.key.size = unit(0.5, 'cm'))

ggsave(paste0(wd,"Code/Archive/plot/","mortality_plot1.png"),width = 13,height = 7)
