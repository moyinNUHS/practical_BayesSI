plot_coef_distrib <- function(outputs, .font_size = font_size, .pt_size = pt_size, N_iter, ind, method, Scenario) {
  
  ##create dfs for storing data 
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
    
      
      iter_data <- as.data.frame(rbind(subset_iter_m1, subset_iter_m1_Wk, subset_iter_m1_Str, subset_iter_m1_Str_UR1, subset_iter_m1_Str_UR2))
      
      ##only includes plotting for methods 1 at the moment --can modify if we want to visualise all 
      
      if(Scenario == "1.6" | Scenario == "1.1"){
      meths <- as.data.frame(c("Method 1", "Method 1 Strong", "Method 1 Strong UR1", "Method 1 Strong UR2"))
      }else{
      meths <- as.data.frame(c("Method 1", "Method 1 Weak", "Method 1 Strong", "Method 1 Strong UR1", "Method 1 Strong UR2"))
      }
      
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
  
plotting <- coef_data 
plotting[,1:4] <- plotting[,1:4] * 100 ##tranform to %


####subsetting to one method -- plot 1 method at a time (specified in input)


plotting <- plotting[plotting$Method == method,]
  
  
  f1 <- ggplot(plotting, aes(x = SampleSize, y = treatment1, 
                             group = SampleSize)) + geom_boxplot(aes()) + theme_minimal() +
    theme(
      plot.title.position = "panel",
      legend.position = "right",
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5)) + labs(
        linetype = NULL,
        shapes = NULL,
        x = "Sample Size",
        y = "Estimated Risk (%)"
      ) + ggtitle("Treatment 1")
  
  f2 <- ggplot(plotting, aes(x = SampleSize, y = treatment2, 
                             group = SampleSize)) + geom_boxplot(aes()) + theme_minimal() +
    theme(
      plot.title.position = "panel",
      legend.position = "right",
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5)) + labs(
        linetype = NULL,
        shapes = NULL,
        x = "Sample Size",
        y = "Estimated Risk (%)"
      ) + ggtitle("Treatment 2")
  
  f3 <- ggplot(plotting, aes(x = SampleSize, y = treatment3, 
                             group = SampleSize)) + geom_boxplot(aes()) + theme_minimal() +
    theme(
      plot.title.position = "panel",
      legend.position = "right",
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5)) + labs(
        linetype = NULL,
        shapes = NULL,
        x = "Sample Size",
        y = "Estimated Risk (%)"
      ) + ggtitle("Treatment 3")
  
  f4 <- ggplot(plotting, aes(x = SampleSize, y = treatment4, 
                             group = SampleSize)) + geom_boxplot(aes()) + theme_minimal() +
    theme(
      plot.title.position = "panel",
      legend.position = "right",
      panel.grid.major.x = element_blank(),
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5)) + labs(
        linetype = NULL,
        shapes = NULL,
        x = "Sample Size",
        y = "Estimated Risk (%)"
      ) + ggtitle("Treatment 4")
  
  

  require(patchwork)

  plots <- f1 + f2 + f3 + f4 + plot_annotation(title = method) ##return 1 plot per treatment -- combine to display together 
  
 return(plots)
 
}
