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
names(colors) = c("treatment A", "treatment B", "treatment C", "treatment D")

font_size = 8
pt_size = 1
N_iter = 1000

### SM 1 would be the Bias for all 15 scenarios ######
bias_list <- list()
inds <- c('1.1', '1.2', '1.3', '1.4', '1.5', '1.6', '2.1', '2.2', '2.3', '2.4', '3.1', '3.2', '4.1', '4.2', '4.3')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_biasmse(Scenario = ind, d = outputs[[ind_name]], .metric = "Bias of treatment contrasts (%)", 
                 name.y = "Bias of treatment contrasts (%)",range.y = c(-0.03,0.01))
    bias_list[[ind]] <- p
  } else {
    message('There are multiple output files in `output`. Please select one.')
  }
}
combined_plot1 <- wrap_plots(bias_list[1:7], ncol = 1) + 
  plot_annotation(tag_levels = 'A',
                  caption = "Fig. SM 1 Bias in each treatment (A-D) coefficient for fifteen scenarios, when using a fixed and mixed effects model \n 
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(legend.position = "bottom",
        plot.caption = element_text(hjust = 0.5)) 
ggsave("./Code/Plots/bias1.pdf", plot = combined_plot1, width = 210 / 25.4, height = 297 / 25.4)
combined_plot2 <- wrap_plots(bias_list[8:15], ncol = 1) + 
  plot_annotation(tag_levels = list(c('H','I','J','K','L','M','N','O')),
                  caption = "Fig. SM 1 Bias in each treatment (A-D) coefficient for fifteen scenarios, when using a fixed and mixed effects model \n 
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("./Code/Plots/bias2.pdf", plot = combined_plot2, width = 210 / 25.4, height = 297 / 25.4)

### SM 2 would be the Coverage probability for all 15 scenarios ####
cover_list <- list()
inds <- c('1.1', '1.2', '1.3', '1.4', '1.5', '1.6', '2.1', '2.2', '2.3', '2.4', '3.1', '3.2', '4.1', '4.2', '4.3')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_biasmse(Scenario = ind, d = outputs[[ind_name]], .metric = "Coverage probability (%)", 
                      name.y = "Coverage probability (%)",range.y = c(0.75,0.95))
    cover_list[[ind]] <- p
  } else {
    message('There are multiple output files in `output`. Please select one.')
  }
}
combined_plot1 <- wrap_plots(cover_list[1:7], ncol = 1) + 
  plot_annotation(tag_levels = 'A',
                  caption = "Fig. SM 2 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for fifteen scenarios, \n
                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("./Code/Plots/cover1.pdf", plot = combined_plot1, width = 210 / 25.4, height = 297 / 25.4)
combined_plot2 <- wrap_plots(cover_list[8:15], ncol = 1) + 
  plot_annotation(tag_levels = list(c('H','I','J','K','L','M','N','O')),
                  caption = "Fig. SM 2 Coverage probability of each treatment (A-D) coefficient 80% confidence/credible intervals for fifteen scenarios, \n
                  when using a fixed and mixed effects model using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("./Code/Plots/cover2.pdf", plot = combined_plot2, width = 210 / 25.4, height = 297 / 25.4)
#### SM 3 would be the MSE for all 15 scenarios #####
mse_list <- list()
inds <- c('1.1', '1.2', '1.3', '1.4', '1.5', '1.6', '2.1', '2.2', '2.3', '2.4', '3.1', '3.2', '4.1', '4.2', '4.3')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_biasmse(Scenario = ind, d = outputs[[ind_name]], .metric = "Mean squared error (%)", 
                      name.y = "Mean squared error (%)",range.y = c(0,0.01))
    mse_list[[ind]] <- p
  } else {
    message('There are multiple output files in `output`. Please select one.')
  }
}
combined_plot1 <- wrap_plots(mse_list[1:7], ncol = 1) + 
  plot_annotation(tag_levels = 'A',
                  caption = "Fig. SM 3 Mean squared error in each treatment (A-D) coefficient for fifteen scenarios, when using a fixed and mixed effects model \n
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("./Code/Plots/mse1.pdf", plot = combined_plot1, width = 210 / 25.4, height = 297 / 25.4)
combined_plot2 <- wrap_plots(mse_list[8:15], ncol = 1) + 
  plot_annotation(tag_levels = list(c('H','I','J','K','L','M','N','O')),
                  caption = "Fig. SM 3 Mean squared error in each treatment (A-D) coefficient for fifteen scenarios, when using a fixed and mixed effects model \n
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("./Code/Plots/mse2.pdf", plot = combined_plot2, width = 210 / 25.4, height = 297 / 25.4)
### type-I error ###
####### SM 4 Type I error for Scens 1.1, 2.1, 2.3, 3.1, 4.1 #####
#Identified 1 best treatment (terminating trial for efficacy)                           
#Identified 2 better treatments (dropping 2 treatments)                                 
#Identified 3 better treatments (dropping 1 treatment)                                  
#Identified any treatment(s) as better than other treatment(s) but in 1 contiguous group

type1_list <- list()
inds <- c('1.1', '2.1', '2.3', '3.1', '4.1')
plot.d <- list()
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    t1_filename <- paste0('t1_', sub('.*/', '', files[[ind_name]]))
    plot.d[[ind]] <- get_type1(Scenario = ind, d = outputs[[ind_name]])
    saveRDS(plot.d, paste0(wd, "Code/Run_output/t1_", sub('.*/', '', files[[ind_name]])))

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
combined_plot <- wrap_plots(type1_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A',
                  caption = "Fig. SM 4 Probability of incorrect interval separation for five null scenarios, when using a fixed and mixed effects model \n
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 
ggsave("./Code/Plots/type1.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

#### SM 5 Type I error (probability of choosing each treatment as best) for Scens 1.1, 2.1, 2.3, 3.1, 4.1 ####
NB_list <- list()
inds <- c('1.1', '2.1', '2.3','3.1', '4.1')
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    p <- plot_nullbest(outputs, font_size, pt_size, N_iter, ind = ind_name)
    NB_list[[ind]] <- p
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}

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
 
combined_plot1 <- wrap_plots(NB_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A',
                  caption = "Fig. SM 5 Proportion of scenarios where each treatment (A-D) were incorrectly predicted as best for five null scenarios, when using a fixed and mixed effects model \n
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("./Code/Plots/NB.pdf", plot = combined_plot1, width = 210 / 25.4, height = 297 / 25.4)


combined_plot2 <- wrap_plots(RAO_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A',
                  caption = "Fig. SM 6 Reduction in adverse outcomes for seven scenarios, when using a fixed and mixed effects model \n
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("./Code/Plots/RAO.pdf", plot = combined_plot2, width = 210 / 25.4, height = 297 / 25.4)


### SM 7 BTP for Scens 1.3-1.5, 2.2, 2.4, 3.2, 4.3 #####
BTP_list <- list()
inds <- c('1.3', '1.4', '1.5','2.2', '2.4', '3.2', '4.3')
plot.d.2 <- list()
for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
      plot.d.2[[ind]] <- get_type2(Scenario = ind, d = outputs[[ind_name]])
      BTP_list[[ind]] <- plot_best(Scenario = ind, plot.d.2[[ind]], name.y = )
  } else {
    message('There are no or multiple output files in `output`. Please select one.')
  }
}
combined_plot <- wrap_plots(BTP_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A',
                  caption = "Fig. SM 7 Best treatment probability for seven scenarios, when using a fixed and mixed effects model \n
                  using frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 
ggsave("./Code/Plots/BTP.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

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
  plot_annotation(tag_levels = 'A',
                  caption = "Fig. SM 8 Improvement over random choice for seven scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 
ggsave("./Code/Plots/NBTP.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

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
  plot_annotation(tag_levels = 'A',
                  caption = "Fig. SM 9 Probability of interval separation for seven scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 
ggsave("./Code/Plots/IORC.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)


### SM 9 PIS (power) for Scens 1.3-1.5, 2.2, 2.4, 3.2, 4.3 ####
### "Either of bottom 2 pre-defined worst treatment identified as worst","Either of top 2 pre-defined best treatment identified as best"     
# "Pre-defined best treatment identified as best","Pre-defined worst treatment identified as worst" 
PIS_list <- list()
inds <- c('1.3', '1.4', '1.5','2.2', '2.4', '3.2', '4.3')

for (ind in inds) {
  ind_name = grep(ind, files,fixed = TRUE)
  if (length(ind_name) == 1) {
    #plot.d = get_type2(Scenario = ind, d = outputs[[ind_name]])
    PIS_list[[paste0(ind,1)]] <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric="Either of bottom 2 pre-defined worst treatment identified as worst",name.y=NULL)
    PIS_list[[paste0(ind,2)]] <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric="Either of top 2 pre-defined best treatment identified as best",name.y=NULL)
    PIS_list[[paste0(ind,3)]] <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric="Pre-defined best treatment identified as best",name.y=NULL)
    PIS_list[[paste0(ind,4)]] <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric="Pre-defined worst treatment identified as worst",name.y=NULL)
  } else {
    message('There are multiple output files in `output`. Please select one.')
  }
}

combined_plot <- wrap_plots(PIS_list[1:12], ncol = 2) + 
  plot_annotation(tag_levels = list('A1','A2','A3','A4','B1','B2','B3','B4','C1','C2','C3','C4'),
                  caption = "Fig. SM 10 Near-best treatment probability for seven scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 


#combined_plot <-  ((PIS_list[[1]]/PIS_list[[2]]|PIS_list[[3]]/PIS_list[[4]])+ plot_layout(tag_level = 'new',axis_titles = "collect")) /
#  ((PIS_list[[5]]/PIS_list[[6]]|PIS_list[[7]]/PIS_list[[8]])+ plot_layout(tag_level = 'new',axis_titles = "collect"))/
#  ((PIS_list[[9]]/PIS_list[[10]]|PIS_list[[11]]/PIS_list[[12]]) + plot_layout(tag_level = 'new',axis_titles = "collect"))+
#  plot_layout(guides = "collect",
#              axes = "collect",
#              axis_titles = "collect")+ 
#  plot_annotation(tag_levels = c('A','1'),tag_sep = '.')&
#  theme(
#    legend.position = "bottom") 
ggsave("./Code/Plots/PIS1.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4 )
#combined_plot <-    ((PIS_list[[13]]/PIS_list[[14]]|PIS_list[[15]]/PIS_list[[16]])+ plot_layout(tag_level = 'new',axis_titles = "collect")) /
#  ((PIS_list[[17]]/PIS_list[[18]]|PIS_list[[19]]/PIS_list[[20]])+ plot_layout(tag_level = 'new',axis_titles = "collect"))/
#  ((PIS_list[[21]]/PIS_list[[22]]|PIS_list[[23]]/PIS_list[[24]]) + plot_layout(tag_level = 'new',axis_titles = "collect"))+ 
#  plot_layout(guides = "collect",
#              axes = "collect",
#              axis_titles = "collect")+ 
#  plot_annotation(tag_levels = list(c('D', 'E', 'F'), '1'),tag_sep = '.')&
#  theme(
#    legend.position = "bottom") 
combined_plot <- wrap_plots(PIS_list[13:24], ncol = 2) + 
  plot_annotation(tag_levels = list('D1','D2','D3','D4','E1','E2','E3','E4','F1','F2','F3','F4'),
                  caption = "Fig. SM 10 Near-best treatment probability for seven scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 

ggsave("./Code/Plots/PIS2.pdf", plot = combined_plot, width = 210 / 25.4 , height = 297 / 25.4)
#combined_plot <-    ((PIS_list[[25]]/PIS_list[[26]]|PIS_list[[27]]/PIS_list[[28]])+ plot_layout(tag_level = 'new',axis_titles = "collect")) + 
#  plot_layout(guides = "collect",
#              axes = "collect",
#              axis_titles = "collect")+ 
#  plot_annotation(tag_levels = list(c('G'), '1'),tag_sep = '.')&
#  theme(
#    legend.position = "bottom") 
combined_plot <- wrap_plots(PIS_list[25:28], ncol = 2) + 
  plot_annotation(tag_levels = list('G1','G2','G3','G4','H1','H2','H3','H4'),
                  caption = "Fig. SM 10 Near-best treatment probability for seven scenarios, when using a fixed and mixed effects model using \n
                  frequentist and Bayesian approaches for sample sizes ranging between N = 500-5,000.
")+
  plot_layout(guides = "collect",
              axes = "collect",
              axis_titles = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 
ggsave("./Code/Plots/PIS3.pdf", plot = combined_plot, width = 297 / 25.4 , height = 210 / 25.4)

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
  plot.d.2[[ind]] = get_type2(Scenario = ind, d = outputs[[ind_name]])
  figure3_list[[2]] <- plot_best(Scenario = ind, plot.d.2[[ind]],name.y="BTP")
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
 
  p <- plot_type2(Scenario = ind, plot.d.2[[ind]], name.y="Power")
  figure3_list[[5]] <- p
} else {
  message('There are multiple output files in `output`. Please select one.')
}

combined_plot <- wrap_plots(figure3_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A',
                  caption = "")+
  plot_layout(guides = "collect",
              axes = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5)) 
ggsave("./Code/Plots/figure3.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)


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
  plot.d.2[[ind]] <- get_type2(Scenario = ind, d = outputs[[ind_name]])
  figure4_list[[2]] <- plot_best(Scenario = ind, plot.d.2[[ind]],name.y="BTP")
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
# "Pre-defined worst treatment identified as worst"
if (length(ind_name) == 1) {
  #plot.d.2[[ind]] = get_type2(Scenario = ind, d = outputs[[ind_name]])
  #p1 <- plot_type2(Scenario = ind, plot.d,.metric = "1 or more best treatments identified as better than worst",name.y="Power 1")
  #figure4_list[[5]] <- p1
  
  p1 <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Pre-defined worst treatment identified as worst",name.y="Power 2")
  figure4_list[[5]] <- p1
} else {
  message('There are multiple output files in `output`. Please select one.')
}
combined_plot <- wrap_plots(figure4_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A',
                  caption = "")+
  plot_layout(guides = "collect",
              axes = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 
ggsave("./Code/Plots/figure4.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)

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
  plot.d.2[[ind]] <- get_type2(Scenario = ind, d = outputs[[ind_name]])
  figure5_list[[2]] <- plot_best(Scenario = ind, plot.d.2[[ind]],name.y="BTP")
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
### "Either of bottom 2 pre-defined worst treatment identified as worst","Either of top 2 pre-defined best treatment identified as best"     
# "Pre-defined best treatment identified as best","Pre-defined worst treatment identified as worst" 
if (length(ind_name) == 1) {
  #plot.d = get_type2(Scenario = ind, d = outputs[[ind_name]])
  p1 <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Either of bottom 2 pre-defined worst treatment identified as worst",name.y="Power 1")
  figure5_list[[5]] <- p1
  
  p2 <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Either of top 2 pre-defined best treatment identified as best",name.y="Power 2")
  figure5_list[[6]] <- p2
  
  p3 <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Pre-defined best treatment identified as best",name.y="Power 3")
  figure5_list[[7]] <- p3
  
  p4 <- plot_type2(Scenario = ind, plot.d.2[[ind]],.metric = "Pre-defined worst treatment identified as worst",name.y="Power 4" )
  figure5_list[[8]] <- p4
  
} else {
  message('There are multiple output files in `output`. Please select one.')
}
combined_plot <- wrap_plots(figure5_list, ncol = 1) + 
  plot_annotation(tag_levels = 'A',
                  caption = "")+
  plot_layout(guides = "collect",
              axes = "collect")&
  theme(
    legend.position = "bottom",
    plot.caption = element_text(hjust = 0.5) ) 
ggsave("./Code/Plots/figure5.pdf", plot = combined_plot, width = 210 / 25.4, height = 297 / 25.4)





