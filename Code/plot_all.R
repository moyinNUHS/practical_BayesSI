library(patchwork)

wd = "/Users/cheryl/Documents/Documents - Xinru’s MacBook Pro (2)/duke-nus/bibhas/practical/practical/"
#wd = '~/Documents/GitHub/practical/'
setwd(wd)
scripts = paste0(wd, 'Code/Functions/', list.files(paste0(wd, 'Code/Functions/')))
lapply(scripts, source)
files = list.files(path = paste0(wd, 'Code/Run_output'), full.names = TRUE, pattern = "iter1000_2024-02-04.rds")
power_t1_files = grep('power_|t1_', files)
if (length(power_t1_files) > 0) {files = files[-grep('power_|t1_', files)]}
outputs = list()
for (file in files){
  outputs[[sub('.*/', '', file)]] = readRDS(file)
}
all_names = names(outputs[[1]][[1]]$scenario_out[[1]])
all_method_names = all_names[grep('method', all_names)]

method_labs = rep(NA, length(all_method_names))
method_labs[grep('method_1', all_method_names)] = "FE "
method_labs[grep('method_1_', all_method_names)] = "FE Bayes "
method_labs[grep('method_2', all_method_names)] = "ME "
method_labs[grep('method_2_', all_method_names)] = "ME Bayes "
method_labs[grep('NI', all_method_names)] = paste0(method_labs[grep('NI', all_method_names)], 'non-informative prior)')
method_labs[endsWith(all_method_names, 'str')] = paste0(method_labs[endsWith(all_method_names, 'str')], 'Rep')
method_labs[endsWith(all_method_names, 'wk')] = paste0(method_labs[endsWith(all_method_names, 'wk')], 'weakly-informative prior)')
method_labs[endsWith(all_method_names, 'str_ur1')] = paste0(method_labs[endsWith(all_method_names, 'str_ur1')], 'URep1')
method_labs[endsWith(all_method_names, 'str_ur2')] = paste0(method_labs[endsWith(all_method_names, 'str_ur2')], 'URep2')
method_labs[endsWith(all_method_names, 'wk_ur1')] = paste0(method_labs[endsWith(all_method_names, 'wk_ur1')], 'weakly-informative/UR1 prior)')
method_labs[endsWith(all_method_names, 'wk_ur2')] = paste0(method_labs[endsWith(all_method_names, 'wk_ur2')], 'weakly-informative/UR2 prior)')
n_methods = length(method_labs)
shapes = 1:n_methods # methods differentiated by shapes 
names(shapes) = method_labs

# get names of treatments
tx_labs = rownames(outputs[[1]][[1]]$scenario_out[[1]]$est_method_1_str)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
colors_list = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))) 
colors = c("black", "#56B4E9", "#009E73", "#E69F00")
#sample(colors_list, length(tx_labs))
names(colors) = tx_labs

font_size = 10
pt_size = 1
N_iter = 1000

### SM 1 would be the Bias for all 15 scenarios ######
bias_list <- list()
inds <- c('1.1', '1.2', '1.3', '1.4', '1.5', '1.6', '2.1', '2.2', '2.3', '2.4', '3.1', '3.2', '4.1', '4.2', '4.3')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_biasmse(Scenario = ind, d = outputs[[ind_name]], .metric = "Bias of treatment contrasts (%)", 
                 name.y = "Bias of treatment contrasts (%)",range.y = c(-0.02,0.005))
    bias_list[[ind]] <- p
  } else {
    message('There are multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(bias_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
        legend.position = "bottom") 
ggsave("./Plots/bias.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

### SM 2 would be the Coverage probability for all 15 scenarios ####
cover_list <- list()
inds <- c('1.1', '1.2', '1.3', '1.4', '1.5', '1.6', '2.1', '2.2', '2.3', '2.4', '3.1', '3.2', '4.1', '4.2', '4.3')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_biasmse(Scenario = ind, d = outputs[[ind_name]], .metric = "Coverage probability (%)", 
                      name.y = "Coverage probability (%)",range.y = c(0.8,0.9))
    cover_list[[ind]] <- p
  } else {
    message('There are multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(cover_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom") 
ggsave("./Plots/cover.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

#### SM 3 would be the MSE for all 15 scenarios #####
mse_list <- list()
inds <- c('1.1', '1.2', '1.3', '1.4', '1.5', '1.6', '2.1', '2.2', '2.3', '2.4', '3.1', '3.2', '4.1', '4.2', '4.3')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_biasmse(Scenario = ind, d = outputs[[ind_name]], .metric = "Coverage probability (%)", 
                      name.y = "Coverage probability (%)",range.y = c(0.8,0.9))
    mse_list[[ind]] <- p
  } else {
    message('There are multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(mse_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom") 
ggsave("./Plots/mse.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

### type-I error ###
####### SM 4 Type I error for Scens 1.1, 2.1, 2.3, 3.1, 4.1 #####
#Identified 1 best treatment (terminating trial for efficacy)                           
#Identified 2 better treatments (dropping 2 treatments)                                 
#Identified 3 better treatments (dropping 1 treatment)                                  
#Identified any treatment(s) as better than other treatment(s) but in 1 contiguous group

type1_list <- list()
inds <- c('1.1', '2.1', '2.3', '3.1', '4.1')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    t1_filename <- paste0('t1_', sub('.*/', '', files[[ind_name]]))
    plot.d <- get_type1(Scenario = ind, d = outputs[[ind_name]])
    #saveRDS(plot.d, paste0(wd, "Code/Run_output/t1_", sub('.*/', '', files[[ind_name]])))

    #plot 
    p <- plot_type1(plot.d,.metric=c("Identified 1 best treatment (terminating trial for efficacy)",
                                "Identified 2 better treatments (dropping 2 treatments)",
                                "Identified 3 better treatments (dropping 1 treatment)"),range.y = c(0,0.2),
                    .label = c("Identified 1 best treatment",
                                "Identified 2 better treatments",
                                "Identified 3 better treatments"))
    type1_list[[ind]] <- p 
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(type1_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom") 
ggsave("./Plots/type1.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

#### SM 5 Type I error (probability of choosing each treatment as best) for Scens 1.1, 2.1, 2.3, 3.1, 4.1 ####
#### SM 6 RAO (mortality gain ratio) for Scens 1.3-1.5, 2.2, 2.4, 3.2, 4.3 ####
## "mortality_gain"       "mortality_gain_ratio" "better_treatment_I"   "nearbest_treatment_5" "diff_min"   
RAO_list <- list()
inds <- c('1.3', '1.4', '1.5','2.2', '2.4', '3.2', '4.3')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="RAO",range.y = c(0.5,1))
    RAO_list[[ind]] <- p 
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(RAO_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom") 
ggsave("./Plots/RAO.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)


### SM 7 BTP for Scens 1.3-1.5, 2.2, 2.4, 3.2, 4.3 #####
BTP_list <- list()
inds <- c('1.3', '1.4', '1.5','2.2', '2.4', '3.2', '4.3')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "better_treatment_I", name.y="BTP",range.y = c(0.8,1))
    BTP_list[[ind]] <- p 
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(BTP_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom") 
ggsave("./Plots/BTP.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

### SM 8 NBTP for Scens 1.3-1.5, 2.2, 2.4, 3.2, 4.3 ####
NBTP_list <- list()
inds <- c('1.3', '1.4', '1.5','2.2', '2.4', '3.2', '4.3')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "nearbest_treatment_5", name.y="NBTP",range.y = c(0.8,1))
    NBTP_list[[ind]] <- p 
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(NBTP_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom") 
ggsave("./Plots/NBTP.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

#### SM 10 IORC (better treatment probability) for Scens 1.3-1.5, 2.2, 2.4, 3.2, 4.3 ####
IORC_list <- list()
inds <- c('1.3', '1.4', '1.5','2.2', '2.4', '3.2', '4.3')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "better_treatment_I", name.y="IORC",range.y = c(0.65,1))
    IORC_list[[ind]] <- p 
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(IORC_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom") 
ggsave("./Plots/IORC.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)


### SM 9 PIS (power) for Scens 1.3-1.5, 2.2, 2.4, 3.2, 4.3 ####
### "Either of bottom 2 pre-defined worst treatment identified as worst","Either of top 2 pre-defined best treatment identified as best"     
# "Pre-defined best treatment identified as best","Pre-defined worst treatment identified as worst" 
PIS_list <- list()
inds <- c('1.3', '1.4', '1.5','2.2', '2.4', '3.2', '4.3')

for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    plot.d = get_type2(Scenario = ind, d = outputs[[ind_name]])
    p <- plot_type2(Scenario = ind, plot.d)
    PIS_list[[ind]] <- p
  } else {
    message('There are multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(PIS_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom") 
ggsave("./Plots/PIS.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

#### Figure 3 A-E RAO, BTP, NBTP, IORC and PIS scenario 1.2 ####
ind <- '1.2'
figure3_list <- list()
ind_name = grep(ind, files,fixed = TRUE)
## RAO
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="RAO",range.y = c(0.5,1))
  figure3_list[[1]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## BTP
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="BTP",range.y = c(0.5,1))
  figure3_list[[2]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## NBTP
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "nearbest_treatment_5", name.y="NBTP",range.y = c(0.8,1))
  figure3_list[[3]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## IORC
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "better_treatment_I", name.y="IORC",range.y = c(0.65,1))
  figure3_list[[4]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## PIS
if (length(ind_name) == 1) {
  plot.d = get_type2(Scenario = ind, d = outputs[[ind_name]])
  p <- plot_type2(Scenario = ind, plot.d)
  figure3_list[[5]] <- p
} else {
  message('There are multiple output files in `output`. Please select one.')
}
combined_plot <- wrap_plots(figure3_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect")&
  theme(
    legend.position = "bottom") 
ggsave("./Plots/figure3.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)


#### Figure 4 A-E RAO, BTP, NBTP, IORC and PIS scenario 1.6 ####
ind <- '1.6'
figure4_list <- list()
ind_name = grep(ind, files,fixed = TRUE)
## RAO
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="RAO",range.y = c(0.5,1))
  figure4_list[[1]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## BTP
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="BTP",range.y = c(0.5,1))
  figure4_list[[2]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## NBTP
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "nearbest_treatment_5", name.y="NBTP",range.y = c(0.8,1))
  figure4_list[[3]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## IORC
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "better_treatment_I", name.y="IORC",range.y = c(0.65,1))
  figure4_list[[4]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## PIS
if (length(ind_name) == 1) {
  plot.d = get_type2(Scenario = ind, d = outputs[[ind_name]])
  p <- plot_type2(Scenario = ind, plot.d)
  figure4_list[[5]] <- p
} else {
  message('There are multiple output files in `output`. Please select one.')
}
combined_plot <- wrap_plots(figure4_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect")&
  theme(
    legend.position = "bottom") 
ggsave("./Plots/figure4.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

#### Figure 5 A-E RAO, BTP, NBTP, IORC and PIS scenario 4.2 ####
ind <- '4.2'
figure5_list <- list()
ind_name = grep(ind, files,fixed = TRUE)
## RAO
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="RAO",range.y = c(0.5,1))
  figure5_list[[1]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## BTP
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="BTP",range.y = c(0.5,1))
  figure5_list[[2]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## NBTP
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "nearbest_treatment_5", name.y="NBTP",range.y = c(0.8,1))
  figure5_list[[3]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## IORC
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "better_treatment_I", name.y="IORC",range.y = c(0.65,1))
  figure5_list[[4]] <- p 
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## PIS
if (length(ind_name) == 1) {
  plot.d = get_type2(Scenario = ind, d = outputs[[ind_name]])
  p <- plot_type2(Scenario = ind, plot.d)
  figure5_list[[5]] <- p
} else {
  message('There are multiple output files in `output`. Please select one.')
}
combined_plot <- wrap_plots(figure5_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A')+
  plot_layout(guides = "collect",
              axes = "collect")&
  theme(
    legend.position = "bottom") 
ggsave("./Plots/figure5.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)





