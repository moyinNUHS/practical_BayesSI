plot_nullbest_alt <- function(outputs, .font_size = font_size, .pt_size = pt_size, N_iter, ind) {
  
  null_data <- as.data.frame(matrix(NA, nrow = 1, ncol = 6)) ##create empty dataframes to store data
  alliter_data <- as.data.frame(matrix(NA, nrow = 1, ncol = 6))
  colnames(null_data) <- c("pattern 1", "pattern 2", "pattern 3", "pattern 4", "Method", "SampleSize")
  colnames(alliter_data) <- c("pattern 1", "pattern 2", "pattern 3", "pattern 4", "Method", "SampleSize")
  
  ###sample sizes vector 
  n = parse_number(names(outputs[[ind]]))
  ##subset to scenario 
  d <- outputs[[ind]]
  
  for (i in n) {
    
    sub_ind <- grep(paste("=",as.character(i), sep=" "), names(d)) 
    subset_n = d[[sub_ind[1]]]$scenario_out ##subset by sample size 
    
    for(a in 1:N_iter){
      
      subset_iter = as.data.frame(subset_n[[a]]$identified_best_t) ##subset by iter
      
      subset_iter$Method <- rownames(subset_iter) ##record methods in column 
      
      subset_iter$SampleSize <- i ##record sample size in column 
      
      
      alliter_data <- rbind(alliter_data, subset_iter) ##combine iterations into one df 
      
    }
    
    null_data <-  rbind(null_data, alliter_data)  ##combine sample sizes into one df
    
    
    
  }
  
  
  null_data <- null_data[!is.na(null_data$`pattern 1`),] ##remove NAs 
  
  plotting <- as.data.frame(matrix(NA, nrow = 1, ncol = 6))
  colnames(plotting) <- c("Treatment 1", "Treatment 2", "Treatment 3", "Treatment 4", "Method", "Sample Size")
  plotm1 <- plotting[1,] ##create initial binding row 
  
  for(q in n){
    for (m in unique(null_data$Method)){
      sample_meth <- subset(null_data, SampleSize == q & Method == m)
      ##subset for each method and sample size 
      plotting[1,1] <- sum(sample_meth[, c(2,4)] == 1) / (nrow(sample_meth) *4) #* 2) 
      plotting[1,2] <- sum(sample_meth[, 1:4] == 2) / (nrow(sample_meth) * 4) ###sum number of occurrences of T2 over # of patterns that T2 is present in (=2)
      plotting[1,3] <- sum(sample_meth[, 1:4] == 3) / (nrow(sample_meth) * 4)
      plotting[1,4] <- sum(sample_meth[, 3:4] == 4) / (nrow(sample_meth) *4) #* 2) ##Currently does not take into account # of avail treatments to keep probability sum = 1 
      plotting[1,5] <- m ##record method 
      plotting[1,6] <- q ##record sample size 
      
      
      plotm1 <- rbind(plotm1, plotting)
    }
  }
  
  
  
  plotm1 <- plotm1[!is.na(plotm1$`Treatment 1`),] ##remove NAs 
  
  #plotm1[,1:4] <- plotm1[,1:4]/rowSums(plotm1[,1:4])
  
  plotm1 <- reshape2::melt(plotm1, id.var = c("Method", "Sample Size"))
  
  fm <- ggplot(plotm1, aes(x =`Sample Size`, y = value, fill = variable)) +
    geom_bar(position = "dodge", stat = "identity") + scale_x_continuous(breaks = c(1000, 2000, 3000, 4000, 5000))+ facet_grid(. ~ Method)+ylab("Proportion of Bests") + xlab("Sample Size")+labs(fill = NULL)+theme_minimal()+ ylim(0, 1.1)+
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
      panel.border = element_rect(colour = "#4d4d4d", fill=NA, linewidth =0.5),
      panel.grid.major.x = element_blank(), axis.text.x = element_text(size = 5), strip.text = element_text(size = 8)
    )+ 
    guides(color = guide_legend(ncol = 2), 
           shape = guide_legend(ncol = 2)) #+ ggtitle()
  
  return(fm)
  
}