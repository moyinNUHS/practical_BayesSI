plot_nullbest <- function(outputs, .font_size = font_size, .pt_size = pt_size, N_iter, ind) {
  coef_data <- as.data.frame(matrix(NA, nrow = 1, ncol = 6))
  alliter_data <- as.data.frame(matrix(NA, nrow = 1, ncol = 6))
  colnames(coef_data) <- c("treatment1", "treatment2", "treatment3", "treatment4", "Method", "SampleSize")
  colnames(alliter_data) <- c("treatment1", "treatment2", "treatment3", "treatment4", "Method", "SampleSize")
  n = parse_number(names(outputs[[ind]])) ##vector of sample sizes
  d <- outputs[[ind]] ##subset by scenario
  for (i in n) {
    sub_ind <- grep(paste("=",as.character(i), sep=" "), names(d))
    subset_n = d[[sub_ind[1]]]$scenario_out ##to choose first in list (ie, if i == 500, to select 500 and not 1500)
    for(a in 1:N_iter){
      ###extract coefficient estimates from each method
      subset_iter_m1 = subset_n[[a]]$est_method_1[,1]
      subset_iter_m1_Wk = subset_n[[a]]$est_method_1_wk[,1]
      subset_iter_m1_Str = subset_n[[a]]$est_method_1_str[,1]
      subset_iter_m1_Str_UR1 = subset_n[[a]]$est_method_1_str_ur1[,1]
      subset_iter_m1_Str_UR2 = subset_n[[a]]$est_method_1_str_ur2[,1]
      subset_iter_m2 = subset_n[[a]]$est_method_2[,1]
      subset_iter_m2_Wk = subset_n[[a]]$est_method_2_wk[,1]
      subset_iter_m2_Str = subset_n[[a]]$est_method_2_str[,1]
      subset_iter_m2_Str_UR1 = subset_n[[a]]$est_method_2_str_ur1[,1]
      subset_iter_m2_Str_UR2 = subset_n[[a]]$est_method_2_str_ur2[,1]
      iter_data <- as.data.frame(rbind(subset_iter_m1, subset_iter_m1_Wk, subset_iter_m1_Str, subset_iter_m1_Str_UR1, subset_iter_m1_Str_UR2, subset_iter_m2, subset_iter_m2_Wk, subset_iter_m2_Str, subset_iter_m2_Str_UR1, subset_iter_m2_Str_UR2))
      ##only includes plotting for methods 1 at the moment --can modify if we want to visualise all
      meths <- as.data.frame(c("Method 1", "Method 1 Strong", "Method 1 Strong UR1", "Method 1 Strong UR2", "Method 2", "Method 2 Strong", "Method 2 Strong UR1", "Method 2 Strong UR2"))
      colnames(meths) <- "Methods"
      iter_data$Method <- meths[,1]
      iter_data$SampleSize <- i
      alliter_data <- rbind(alliter_data, iter_data) ##combine data for all iterations
      alliter_data <- alliter_data[!is.na(alliter_data$treatment1),]
    }
    coef_data <-  rbind(coef_data, alliter_data) ##combine data for all sample sizes
  }
  coef_data <- coef_data[!is.na(coef_data$treatment1),]
  coef_data$treatment1 <- invlogit(coef_data$treatment1) ##transform coefficients to obtain risk
  coef_data$treatment2 <- invlogit(coef_data$treatment2)
  coef_data$treatment3 <- invlogit(coef_data$treatment3)
  coef_data$treatment4 <- invlogit(coef_data$treatment4)
  coef_data$best_tx <- NA
  for (i in 1:nrow(coef_data)){
    coef_data$best_tx[i] <- min(coef_data[i,1:4])
  }
  coef_data$best_tx_name <- NA
  for (i in 1:nrow(coef_data)){
    if(coef_data$best_tx[i] == coef_data$treatment1[i]){
      coef_data$best_tx_name[i] <- "treatment1"
    } else if(coef_data$best_tx[i] == coef_data$treatment2[i]){
      coef_data$best_tx_name[i] <- "treatment2"
    } else if (coef_data$best_tx[i] == coef_data$treatment3[i]){
      coef_data$best_tx_name[i] <- "treatment3"
    } else{
      coef_data$best_tx_name[i] <- "treatment4"
    }
  }
  null_data <- coef_data[,5:8]
  plotting <- as.data.frame(matrix(NA, nrow = 1, ncol = 6))
  colnames(plotting) <- c("treatment1", "treatment2", "treatment3", "treatment4", "Method", "Sample Size")
  plotm1 <- plotting[1,]
  for(q in n){
    for (m in unique(null_data$Method)){
      sample_meth <- subset(null_data, SampleSize == q & Method == m)
      ##subset for each method and sample size
      plotting[1,1]  <- table(sample_meth$best_tx_name)[1] / nrow(sample_meth)
      plotting[1,2] <- table(sample_meth$best_tx_name)[2] / nrow(sample_meth)
      plotting[1,3] <- table(sample_meth$best_tx_name)[3] / nrow(sample_meth)
      plotting[1,4] <- table(sample_meth$best_tx_name)[4] / nrow(sample_meth)
      plotting[1,5] <- m ##record method
      plotting[1,6] <- q ##record sample size
      plotm1 <- rbind(plotm1, plotting)
    }
  }
  plotm1 <- plotm1[!is.na(plotm1$`treatment1`),] ##remove NAs
  plotm1 <- reshape2::melt(plotm1, id.var = c("Method", "Sample Size"))
  fm <- ggplot(plotm1, aes(x =`Sample Size`, y = value, fill = variable)) +
    geom_col() + scale_x_continuous(breaks = c(1000, 2000, 3000, 4000, 5000))+ facet_grid(. ~ Method)+ylab("Proportion of Bests") + xlab("Sample Size")+labs(fill = NULL)+theme_minimal()+ scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0), limits = c(0, 1.0)) +
    theme(
      plot.title.position = "plot",
      legend.position = "bottom",
      strip.background = element_blank(),
      legend.spacing.y = unit(0.02, 'cm'),
      legend.margin=margin(0,0,0,0),
      text = element_text(size = font_size),
      legend.title = element_text(),
      legend.key.size = unit(0.5, 'cm'),
      panel.spacing = unit(0.05, 'cm'),
      panel.background = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "#4D4D4D", fill=NA, linewidth =0.5),
      panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 5), strip.text = element_text(size = 8)
    )+
    guides(color = guide_legend(ncol = 2),
           shape = guide_legend(ncol = 2))+ scale_fill_manual(values = colors) #+ ggtitle()
  return(fm)
}
