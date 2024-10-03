library(patchwork)
library(RColorBrewer)

wd = "/Users/Simulations/practical-main/Code/" 

scripts = paste0(wd, "UpdatedFunctions/Sim_Functions/", list.files(paste0(wd, "UpdatedFunctions/Sim_Functions/")))
lapply(scripts, source)

setwd(wd)
scripts = paste0(wd, "UpdatedFunctions/Plotting_Functions/", list.files(paste0(wd, "UpdatedFunctions/Plotting_Functions/")))

lapply(scripts, source)

files = list.files(path = paste0(wd, "Final_Output/"), full.names = TRUE)
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
names(colors) = c("treatment A", "treatment B", "treatment C", "treatment D")

font_size = 15
pt_size = 1
N_iter = 1000


#################Main figs: #####################################

#### Figure 3 A-E RAO, BTP, IORC and PIS scenario 1.2 ####
ind <- '1.2'
#figure3_list <- list()
plot.d.2 <- list() 
ind_name = grep(ind, files,fixed = TRUE)

## RAO
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="RMR",range.y = c(0.5,1))
  #figure3_list[[1]] <- p 
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig3A.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}



## BTP
if (length(ind_name) == 1) {
  plot.d.2[[ind]] <- get_type2(Scenario = ind, d = outputs[[ind_name]])
  p <- plot_best(Scenario = ind, plot.d.2[[ind]],name.y="BTP")
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig3B.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}


## IORC
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "better_treatment_I", name.y="IORC",range.y = c(0.7,1))
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig3C.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}


## PIS
if (length(ind_name) == 1) {
  
  p <- plot_type2(Scenario = ind, plot.d.2[[ind]], name.y="Power")
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig3D.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
} else {
  message('There are multiple output files in `output`. Please select one.')
}




#### Figure 4 A-E RAO, BTP, IORC and PIS scenario 1.3 ####
ind <- '1.3'

ind_name = grep(ind, files,fixed = TRUE)
## RAO
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="RMR",range.y = c(0.5,1))
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig4A.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}


## BTP
if (length(ind_name) == 1) {
  plot.d.2[[ind]] <- get_type2(Scenario = ind, d = outputs[[ind_name]])
  p <- plot_best(Scenario = ind, plot.d.2[[ind]],name.y="BTP")
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig4B.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}


## IORC
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "better_treatment_I", name.y="IORC",range.y = c(0.7,1))
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig4C.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}


## PIS


# "Pre-defined worst treatment identified as worst"
if (length(ind_name) == 1) {
  #plot.d.2[[ind]] = get_type2(Scenario = ind, d = outputs[[ind_name]])
  p <- plot_type2(Scenario = ind,plot.d.2[[ind]],.metric = "Either of bottom 2 pre-defined worst treatment identified as worst", name.y="Power 2")
  #figure4_list[[5]] <- p1
  
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig4E.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
  p <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Pre-defined worst treatment identified as worst",name.y="Power 1")
  #figure4_list[[5]] <- p1
  
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig4D.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
  # plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Pre-defined worst treatment identified as worst",name.y="Power 2")
  
} else {
  message('There are multiple output files in `output`. Please select one.')
}


#### Figure 5 A-E RAO, BTP, NBTP, IORC and PIS scenario  ####
ind <- '1.4'
#figure5_list <- list()
ind_name = grep(ind, files,fixed = TRUE)
## RAO
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="RMR",range.y = c(0.5,1))
  #figure5_list[[1]] <- p 
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig5A.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}

## BTP
if (length(ind_name) == 1) {
  plot.d.2[[ind]] <- get_type2(Scenario = ind, d = outputs[[ind_name]])
  p <- plot_best(Scenario = ind, plot.d.2[[ind]],name.y="BTP")
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig5B.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}

## IORC
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "better_treatment_I", name.y="IORC",range.y = c(0.7,1))
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig5C.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## PIS
### "Either of bottom 2 pre-defined worst treatment identified as worst","Either of top 2 pre-defined best treatment identified as best"     
# "Pre-defined best treatment identified as best","Pre-defined worst treatment identified as worst" 
if (length(ind_name) == 1) {
  #plot.d = get_type2(Scenario = ind, d = outputs[[ind_name]])
  # p1 <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Either of bottom 2 pre-defined worst treatment identified as worst",name.y="Power 2")
  #  figure5_list[[5]] <- p1
  
  #p2 <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Either of top 2 pre-defined best treatment identified as best",name.y="Power 2")
  #figure5_list[[6]] <- p2
  
  ####p3 <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Pre-defined best treatment identified as best",name.y="Power 3")
  ### figure5_list[[7]] <- p3
  
  p <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Pre-defined worst treatment identified as worst",name.y="Power 1" )
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig5D.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
  # plot_type2(Scenario = ind, plot.d.2[[ind]], name.y="Power")
  
} else {
  message('There are multiple output files in `output`. Please select one.')
}


###Fig 6 -- Scenario 2.2 

ind <- '2.2' ##check second file is being used!!! 

ind_name = grep(ind, files,fixed = TRUE)
#ind_name <- ind_name[2]
## RAO
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="RMR",range.y = c(0.5,1))
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig6A.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## BTP
if (length(ind_name) == 1) {
  plot.d.2[[ind]] <- get_type2(Scenario = ind, d = outputs[[ind_name]])
  p <- plot_best(Scenario = ind, plot.d.2[[ind]],name.y="BTP")
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig6B.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}

## IORC
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "better_treatment_I", name.y="IORC",range.y = c(0.7,1))
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig6C.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## PIS
### "Either of bottom 2 pre-defined worst treatment identified as worst","Either of top 2 pre-defined best treatment identified as best"     
# "Pre-defined best treatment identified as best","Pre-defined worst treatment identified as worst" 
if (length(ind_name) == 1) {
  #plot.d = get_type2(Scenario = ind, d = outputs[[ind_name]])
  p <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Either of bottom 2 pre-defined worst treatment identified as worst",name.y="Power 2")
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig6E.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
  
  p <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Pre-defined worst treatment identified as worst",name.y="Power 1" )
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig6D.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
} else {
  message('There are multiple output files in `output`. Please select one.')
}


##### Fig 7 -- Scenario 2.3 
ind <- '2.3'
figure5_list <- list()
ind_name = grep(ind, files,fixed = TRUE)
## RAO
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="RMR",range.y = c(0.5,1))
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig7A.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}
## BTP
if (length(ind_name) == 1) {
  plot.d.2[[ind]] <- get_type2(Scenario = ind, d = outputs[[ind_name]])
  p <- plot_best(Scenario = ind, plot.d.2[[ind]],name.y="BTP")
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig7B.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}

## IORC
if (length(ind_name) == 1) {
  p <- plot_mort(Scenario = ind, d = outputs[[ind_name]],.metric = "better_treatment_I", name.y="IORC",range.y = c(0.7,1))
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig7C.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
} else {
  message('There are no or multiple output files in `output`. Please select one.')
}

## PIS
### "Either of bottom 2 pre-defined worst treatment identified as worst","Either of top 2 pre-defined best treatment identified as best"     
# "Pre-defined best treatment identified as best","Pre-defined worst treatment identified as worst" 
if (length(ind_name) == 1) {
  
  p <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Either of bottom 2 pre-defined worst treatment identified as worst",name.y="Power 2")
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig7E.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
  
  p <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Pre-defined worst treatment identified as worst",name.y="Power 1" )
  ggsave("/Users/Simulations/practical-main/Code/RP_Plots/Fig7D.jpeg", plot = p, width = 100 / 25.4, height = 95 / 25.4)
  
} else {
  message('There are multiple output files in `output`. Please select one.')
}







######Supplementary Figs:

##source plotting code for FE and ME 
source(paste0(wd, "UpdatedFunctions/ALL_methods_plot_mort.R"))

source(paste0(wd, "UpdatedFunctions/plot_best_all.R"))

source(paste0(wd, "UpdatedFunctions/plot_type2_all.R"))

font_size = 15

type1_list <- list()
inds <- c('1.1', '2.1', '3.1', '3.3', '4.1')
plot.d <- list()
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    #t1_filename <- paste0('t1_', sub('.*/', '', files[[ind_name]]))
    plot.d[[ind]] <- get_type1(Scenario = ind, d = outputs[[ind_name]])
    #saveRDS(plot.d, paste0(wd, "Code/Run_output/t1_", sub('.*/', '', files[[ind_name]])))
    
    #plot 
    p <- plot_type1(plot.d[[ind]],.metric=c("Identified 1 best treatment (terminating trial for efficacy)",
                                            "Identified 2 better treatments (dropping 2 treatments)",
                                            "Identified 3 better treatments (dropping 1 treatment)"),range.y = c(0,0.05),
                    .label = c("Identified 1 best treatment",
                               "Identified 2 better treatments",
                               "Identified 3 better treatments"))
    type1_list[[ind]] <- p 
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}

combined_plot1 <- wrap_plots(type1_list[1:2], ncol = 1) + 
  plot_annotation(
    caption = "Fig. SM 1 Probability of incorrect interval separation for all null scenarios, when using a fixed and mixed effects model \n
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 


ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM1A.jpeg", plot = combined_plot1, width = 210 / 25.4, height = 145 / 25.4)

combined_plot2 <- wrap_plots(type1_list[3:4], ncol = 1) + 
  plot_annotation(
    caption = "Fig. SM 1 Probability of incorrect interval separation for all null scenarios, when using a fixed and mixed effects model \n
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 


ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM1B.jpeg", plot = combined_plot2, width = 210 / 25.4, height = 145 / 25.4)

combined_plot3 <- wrap_plots(type1_list[4:5], ncol = 1) + 
  #plot_annotation(
  #  caption = "Fig. SM 1 Probability of incorrect interval separation for all null scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 


ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM1C.jpeg", plot = combined_plot3, width = 210 / 25.4, height = 145 / 25.4)


####SM2 

NB_list <- list()
inds <- c('1.1', '2.1', '3.1','3.3', '4.1')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_nullbest(outputs, font_size, pt_size, N_iter, ind = ind_name)
    NB_list[[ind]] <- p
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}


combined_plot1 <- wrap_plots(NB_list, ncol = 1) + 
  plot_annotation(
    caption = "Fig. SM 2 Proportion of scenarios where each treatment (A-D) were incorrectly predicted as best for all null scenarios, when using a fixed and mixed effects model \n
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM2.pdf", plot = combined_plot1, width = 210 / 25.4, height = 297 / 25.4)

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM2.jpeg", plot = combined_plot1, width = 210 / 25.4, height = 297 / 25.4)



####SM3 

font_size = 15

RAO_list <- list()

inds <- c('1.2', '1.3', '1.4', '1.5', '1.6', '2.2', '2.3', '3.2', '3.4', '4.2')

for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_mort_all(Scenario = ind, d = outputs[[ind_name]],.metric = "mortality_gain_ratio", name.y="RMR",range.y = c(0.5,1))
    RAO_list[[ind]] <- p
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}


combined_plot2 <- wrap_plots(RAO_list, ncol = 2) + 
  # plot_annotation(
  #                  caption = "Fig. SM 3 Reduction in mortality ratio for all non-null scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 

combined_plot2

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM3.jpeg", plot = combined_plot2, width = 210 / 25.4, height = 297 / 25.4)

####SM4 

BTP_list <- list()
inds <- c('1.2', '1.3', '1.4', '1.5', '1.6', '2.2', '2.3', '3.2', '3.4', '4.2')
plot.d.2 <- list()
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    plot.d.2[[ind]] <- get_type2(Scenario = ind, d = outputs[[ind_name]])
    BTP_list[[ind]] <- plot_best_all(Scenario = ind, plot.d.2[[ind]], name.y = "BTP")
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(BTP_list, ncol = 2) + 
  plot_annotation(
    caption = "Fig. SM 4 Best treatment probability for all non-null scenarios, when using a fixed and mixed effects model \n
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 

combined_plot

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM4.jpeg", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

##SM5: IORC

IORC_list <- list()
inds <- c('1.2', '1.3', '1.4', '1.5', '1.6', '2.2', '2.3', '3.2', '3.4', '4.2')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_mort_all(Scenario = ind, d = outputs[[ind_name]],.metric = "better_treatment_I", name.y="IORC",range.y = c(0.7,1))
    IORC_list[[ind]] <- p 
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(IORC_list, ncol = 2) + 
  plot_annotation(
    caption = "Fig. SM 5 Improvement over random choice for all non-null scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 

combined_plot

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM5.jpeg", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

###SM 6: P near-best 

NBTP_list <- list()
inds <- c('1.2', '1.3', '1.4', '1.5', '1.6', '2.2', '2.3', '3.2', '3.4', '4.2')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_mort_all(Scenario = ind, d = outputs[[ind_name]],.metric = "nearbest_treatment_5", name.y="NBTP",range.y = c(0.75,1))
    NBTP_list[[ind]] <- p 
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(NBTP_list, ncol = 2) + 
  plot_annotation(
    caption = "Fig. SM 6  Near-best treatment probability for all non-null scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 

combined_plot

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM6.jpeg", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

###SM7 PIS 
PIS_list <- list()
inds <- c('1.2', '1.3', '1.4', '1.5', '1.6', '2.2', '2.3', '3.2', '3.4', '4.2')

for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1 & ind != "1.2" & ind != "1.4") {
    #plot.d = get_type2(Scenario = ind, d = outputs[[ind_name]])
    PIS_list[[paste0(ind,1)]] <- plot_type2_all(Scenario = ind, plot.d.2[[ind]],.metric= "Pre-defined worst treatment identified as worst",name.y="A")
    PIS_list[[paste0(ind,2)]] <- plot_type2_all(Scenario = ind, plot.d.2[[ind]],.metric="Either of bottom 2 pre-defined worst treatment identified as worst",name.y="B")
    PIS_list[[paste0(ind,3)]] <- plot_type2_all(Scenario = ind, plot.d.2[[ind]],.metric="Pre-defined best treatment identified as best",name.y="C")
    PIS_list[[paste0(ind,4)]] <- plot_type2_all(Scenario = ind, plot.d.2[[ind]],.metric="Either of top 2 pre-defined best treatment identified as best",name.y="D")
  } else if (ind == "1.2"){
    
    PIS_list[[ind]] <- plot_type2_all(Scenario = ind, plot.d.2[[ind]], name.y = "PIS")
    
  } else if (ind == "1.4"){
    
    PIS_list[[ind]] <- plot_type2_all(Scenario = ind, plot.d.2[[ind]], name.y = "PIS")
    
  }else {
    message('There are multiple output files in `output`. Please select one.')
  }
}

combined_plot1 <- wrap_plots(PIS_list[1], ncol = 4, nrow = 4) + 
  plot_annotation(
    caption = "Fig. SM 7 Probability of interval separation for all non-null scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 

combined_plot1 

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM7a.jpeg", plot = combined_plot1, width = 210 / 25.4, height = 297 / 25.4)


combined_plot2 <- wrap_plots(PIS_list[2:5], ncol = 4, nrow = 4) + 
  plot_annotation(
    caption = "Fig. SM 7 Probability of interval separation for all non-null scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 


combined_plot2 

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM7b.jpeg", plot = combined_plot2, width = 210 / 25.4, height = 297 / 25.4)


combined_plot3 <- wrap_plots(PIS_list[6], ncol = 4, nrow = 4) + 
  plot_annotation(
    caption = "Fig. SM 7 Probability of interval separation for all non-null scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 


combined_plot3 

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM7c.jpeg", plot = combined_plot3, width = 210 / 25.4, height = 297 / 25.4)


combined_plot4 <- wrap_plots(PIS_list[7:10], ncol = 4, nrow = 4) + 
  plot_annotation(
    caption = "Fig. SM 7 Probability of interval separation for all non-null scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 


combined_plot4 

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM7d.jpeg", plot = combined_plot4, width = 210 / 25.4, height = 297 / 25.4)


combined_plot5 <- wrap_plots(PIS_list[11:26], ncol = 4, nrow = 4) + 
  plot_annotation(
    caption = "Fig. SM 7 Probability of interval separation for all non-null scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 


combined_plot5 

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM7e.jpeg", plot = combined_plot5, width = 210 / 25.4, height = 297 / 25.4)


combined_plot6 <- wrap_plots(PIS_list[27:34], ncol = 4, nrow = 4) + 
  plot_annotation(
    caption = "Fig. SM 7 Probability of interval separation for all non-null scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 


combined_plot6

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM7f.jpeg", plot = combined_plot6, width = 210 / 25.4, height = 297 / 25.4)


##SM 8 Bias 
bias_list <- list()
inds <- c('1.1', '1.2', '1.3', '1.4', '1.5', '1.6', '2.1', '2.2', '2.3', '3.1', '3.2', '3.3', '3.4', '4.1', '4.2')

for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_biasmse(Scenario = ind, d = outputs[[ind_name]], .metric = "Bias of treatment contrasts (%)", 
                      name.y = "Bias of treatment contrasts (%)",range.y = c(-0.1,0.08), .font_size = 20)
    bias_list[[ind]] <- p
  } else {
    message('There are multiple output files in `output`. Please select one.')
  }
}
combined_plot1 <- wrap_plots(bias_list[1], ncol = 1, nrow = 2) + 
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5)) 

combined_plot1

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8a.jpeg", plot = combined_plot1, width = 210 / 25.4, height = 145 / 25.4)

combined_plot2 <- wrap_plots(bias_list[2], ncol = 1, nrow = 2) + 
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8b.jpeg", plot = combined_plot2, width = 210 / 25.4, height = 145 / 25.4)


combined_plot3 <- wrap_plots(bias_list[3], ncol = 1, nrow = 2) + 
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8c.jpeg", plot = combined_plot3, width = 210 / 25.4, height = 145 / 25.4)

combined_plot4 <- wrap_plots(bias_list[4], ncol = 1, nrow = 2) + 
  plot_annotation(caption = "Fig. SM 8 Bias in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n 
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.") +
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5))

ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8d.jpeg", plot = combined_plot4, width = 210 / 25.4, height = 145 / 25.4)

combined_plot5 <- wrap_plots(bias_list[5], ncol = 1, nrow = 2) + 
  plot_annotation(caption = "Fig. SM 8 Bias in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n 
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.") +
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8e.jpeg", plot = combined_plot5, width = 210 / 25.4, height = 145 / 25.4)

combined_plot6 <- wrap_plots(bias_list[6], ncol = 1, nrow = 2) + 
  plot_annotation(caption = "Fig. SM 8 Bias in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n 
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.") +
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8f.jpeg", plot = combined_plot6, width = 210 / 25.4, height = 145 / 25.4)

combined_plot7 <- wrap_plots(bias_list[7], ncol = 1, nrow = 2) + 
  plot_annotation(caption = "Fig. SM 8 Bias in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n 
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.") +
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8g.jpeg", plot = combined_plot7, width = 210 / 25.4, height = 145 / 25.4)

combined_plot8 <- wrap_plots(bias_list[8], ncol = 1, nrow = 2) + 
  plot_annotation(caption = "Fig. SM 8 Bias in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n 
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.") +
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8h.jpeg", plot = combined_plot8, width = 210 / 25.4, height = 145 / 25.4)

combined_plot9 <- wrap_plots(bias_list[9], ncol = 1, nrow = 2) + 
  plot_annotation(caption = "Fig. SM 8 Bias in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n 
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.") +
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8i.jpeg", plot = combined_plot9, width = 210 / 25.4, height = 145 / 25.4)

combined_plot10 <- wrap_plots(bias_list[10], ncol = 1, nrow = 2) + 
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8j.jpeg", plot = combined_plot10, width = 210 / 25.4, height = 145 / 25.4)

combined_plot11 <- wrap_plots(bias_list[11], ncol = 1, nrow = 2) + 
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8k.jpeg", plot = combined_plot11, width = 210 / 25.4, height = 145 / 25.4)


combined_plot12 <- wrap_plots(bias_list[12], ncol = 1, nrow = 2) + 
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8l.jpeg", plot = combined_plot12, width = 210 / 25.4, height = 145 / 25.4)

combined_plot13 <- wrap_plots(bias_list[13], ncol = 1, nrow = 2) + 
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8m.jpeg", plot = combined_plot13, width = 210 / 25.4, height = 145 / 25.4)

combined_plot14 <- wrap_plots(bias_list[14], ncol = 1, nrow = 2) + 
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8n.jpeg", plot = combined_plot14, width = 210 / 25.4, height = 145 / 25.4)

combined_plot15 <- wrap_plots(bias_list[15], ncol = 1, nrow = 2) + 
  plot_annotation(caption = "Fig. SM 8 Bias in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n 
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.") +
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM8o.jpeg", plot = combined_plot15, width = 210 / 25.4, height = 145 / 25.4)


###SM 9 Coverage probability 
cover_list <- list()
inds <- c('1.1', '1.2', '1.3', '1.4', '1.5', '1.6', '2.1', '2.2', '2.3', '3.1', '3.2', '3.3', '3.4', '4.1', '4.2')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_biasmse(Scenario = ind, d = outputs[[ind_name]], .metric = "Coverage probability (%)", 
                      name.y = "Coverage probability (%)",range.y = c(0, 1))
    cover_list[[ind]] <- p
  } else {
    message('There are multiple output files in `output`. Please select one.')
  }
}
combined_plot1 <- wrap_plots(cover_list[1], ncol = 1, nrow = 2) + 
  plot_annotation(
    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9a.jpeg", plot = combined_plot1, width = 210 / 25.4, height = 145 / 25.4)


combined_plot2 <- wrap_plots(cover_list[2], ncol = 1, nrow = 2) + 
  plot_annotation(
    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9b.jpeg", plot = combined_plot2, width = 210 / 25.4, height = 145 / 25.4)

combined_plot3 <- wrap_plots(cover_list[3], ncol = 1, nrow = 2) + 
  plot_annotation(
    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9c.jpeg", plot = combined_plot3, width = 210 / 25.4, height = 145 / 25.4)

combined_plot4 <- wrap_plots(cover_list[4], ncol = 1, nrow = 2) + 
  plot_annotation(
    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9d.jpeg", plot = combined_plot4, width = 210 / 25.4, height = 145 / 25.4)

combined_plot5 <- wrap_plots(cover_list[5], ncol = 1, nrow = 2) + 
  plot_annotation(
    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9e.jpeg", plot = combined_plot5, width = 210 / 25.4, height = 145 / 25.4)

combined_plot6 <- wrap_plots(cover_list[6], ncol = 1, nrow = 2) + 
  plot_annotation(
    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9f.jpeg", plot = combined_plot6, width = 210 / 25.4, height = 145 / 25.4)

combined_plot7 <- wrap_plots(cover_list[7], ncol = 1, nrow = 2) + 
  plot_annotation(
    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9g.jpeg", plot = combined_plot7, width = 210 / 25.4, height = 145 / 25.4)

combined_plot8 <- wrap_plots(cover_list[8], ncol = 1, nrow = 2) + 
  # plot_annotation(
  #    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
  #                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9h.jpeg", plot = combined_plot8, width = 210 / 25.4, height = 145 / 25.4)

combined_plot9 <- wrap_plots(cover_list[9], ncol = 1, nrow = 2) + 
  # plot_annotation(
  #    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
  #                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9i.jpeg", plot = combined_plot9, width = 210 / 25.4, height = 145 / 25.4)

combined_plot10 <- wrap_plots(cover_list[10], ncol = 1, nrow = 2) + 
  # plot_annotation(
  #    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
  #                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9j.jpeg", plot = combined_plot10, width = 210 / 25.4, height = 145 / 25.4)

combined_plot11 <- wrap_plots(cover_list[11], ncol = 1, nrow = 2) + 
  # plot_annotation(
  #    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
  #                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9k.jpeg", plot = combined_plot11, width = 210 / 25.4, height = 145 / 25.4)

combined_plot12 <- wrap_plots(cover_list[12], ncol = 1, nrow = 2) + 
  # plot_annotation(
  #    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
  #                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9l.jpeg", plot = combined_plot12, width = 210 / 25.4, height = 145 / 25.4)

combined_plot13 <- wrap_plots(cover_list[13], ncol = 1, nrow = 2) + 
  # plot_annotation(
  #    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
  #                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9m.jpeg", plot = combined_plot13, width = 210 / 25.4, height = 145 / 25.4)

combined_plot14 <- wrap_plots(cover_list[14], ncol = 1, nrow = 2) + 
  # plot_annotation(
  #    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
  #                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9n.jpeg", plot = combined_plot14, width = 210 / 25.4, height = 145 / 25.4)


combined_plot15 <- wrap_plots(cover_list[15], ncol = 1, nrow = 2) + 
  plot_annotation(
    caption = "Fig. SM 9 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for all scenarios, \n
                    when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  ")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM9o.jpeg", plot = combined_plot15, width = 210 / 25.4, height = 145 / 25.4)

###SM10 MSE 

mse_list <- list()
inds <- c('1.1', '1.2', '1.3', '1.4', '1.5', '1.6', '2.1', '2.2', '2.3', '3.1', '3.2', '3.3', '3.4', '4.1', '4.2')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_biasmse(Scenario = ind, d = outputs[[ind_name]], .metric = "Mean squared error (%)", 
                      name.y = "Mean squared error (%)",range.y = c(0,0.02))
    mse_list[[ind]] <- p
  } else {
    message('There are multiple output files in `output`. Please select one.')
  }
}
combined_plot1 <- wrap_plots(mse_list[1], ncol = 1, nrow = 2) + 
  # plot_annotation(
  #                  caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10a.jpeg", plot = combined_plot1, width = 210 / 25.4, height = 145 / 25.4)

combined_plot2 <- wrap_plots(mse_list[2], ncol = 1, nrow = 2) + 
  # plot_annotation(
  #                caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10b.jpeg", plot = combined_plot2, width = 210 / 25.4, height = 145 / 25.4)

combined_plot3 <- wrap_plots(mse_list[3], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10c.jpeg", plot = combined_plot3, width = 210 / 25.4, height = 145 / 25.4)

combined_plot4 <- wrap_plots(mse_list[4], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10d.jpeg", plot = combined_plot4, width = 210 / 25.4, height = 145 / 25.4)


combined_plot5 <- wrap_plots(mse_list[5], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10e.jpeg", plot = combined_plot5, width = 210 / 25.4, height = 145 / 25.4)

combined_plot6 <- wrap_plots(mse_list[6], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10f.jpeg", plot = combined_plot6, width = 210 / 25.4, height = 145 / 25.4)


combined_plot7 <- wrap_plots(mse_list[7], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10g.jpeg", plot = combined_plot7, width = 210 / 25.4, height = 145 / 25.4)

combined_plot8 <- wrap_plots(mse_list[8], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10h.jpeg", plot = combined_plot8, width = 210 / 25.4, height = 145 / 25.4)


combined_plot9 <- wrap_plots(mse_list[9], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10i.jpeg", plot = combined_plot9, width = 210 / 25.4, height = 145 / 25.4)


combined_plot10 <- wrap_plots(mse_list[10], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10j.jpeg", plot = combined_plot10, width = 210 / 25.4, height = 145 / 25.4)

combined_plot11 <- wrap_plots(mse_list[11], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10k.jpeg", plot = combined_plot11, width = 210 / 25.4, height = 145 / 25.4)


combined_plot12 <- wrap_plots(mse_list[12], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10l.jpeg", plot = combined_plot12, width = 210 / 25.4, height = 145 / 25.4)

combined_plot13 <- wrap_plots(mse_list[13], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10m.jpeg", plot = combined_plot13, width = 210 / 25.4, height = 145 / 25.4)


combined_plot14 <- wrap_plots(mse_list[14], ncol = 1, nrow = 2) + 
  #  plot_annotation(
  #    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
  #                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  #")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10n.jpeg", plot = combined_plot14, width = 210 / 25.4, height = 145 / 25.4)


combined_plot15 <- wrap_plots(mse_list[15], ncol = 1, nrow = 2) + 
  plot_annotation(
    caption = "Fig. SM 10 Mean squared error in each treatment (A-D) coefficient for all scenarios, when using a fixed and mixed effects model \n
                    using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
  ")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("/Users/Simulations/practical-main/Code/RP_Plots/SM10o.jpeg", plot = combined_plot15, width = 210 / 25.4, height = 145 / 25.4)
