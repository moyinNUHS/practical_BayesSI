
plot_nullbest <- function(outputs, .font_size = font_size, .pt_size = pt_size, N_iter, ind) {
  
  null_data <- as.data.frame(matrix(NA, nrow = 1, ncol = 6))
  alliter_data <- as.data.frame(matrix(NA, nrow = 1, ncol = 6))
  colnames(null_data) <- c("Treatment1", "Treatment2", "Treatment3", "Treatment4", "Method", "SampleSize")
  colnames(alliter_data) <- c("Treatment1", "Treatment2", "Treatment3", "Treatment4", "Method", "SampleSize")
  n = parse_number(names(outputs[[ind]]))
  d <- outputs[[ind]]
  subset_iter_fin <- as.data.frame(matrix(NA, nrow = 1, ncol = 6))
  colnames(subset_iter_fin) <- c("Treatment1", "Treatment2", "Treatment3", "Treatment4", "Method", "SampleSize")
  
  for (i in n) {
    
    sub_ind <- grep(paste("=",as.character(i), sep=" "), names(d))
    subset_n = d[[sub_ind[1]]]$scenario_out
    
    for(a in 1:N_iter){
      
      subset_iter = as.data.frame(subset_n[[a]]$identified_best_t)
      
      subset_iter$Method <- rownames(subset_iter)
      
      subset_iter$SampleSize <- i
      
      colnames(subset_iter) <- c("Treatment1", "Treatment2", "Treatment3", "Treatment4", "Method", "SampleSize")
      
      #subset_iter_fin <- rbind(subset_iter_fin, subset_iter)
      #subset_iter_fin <- subset_iter_fin[!is.na(subset_iter_fin$Treatment1),]
      #subset_iter_fin$Method <- rownames(subset_iter_fin)
      iter_data <- as.data.frame(matrix(NA, nrow = nrow(subset_iter), ncol = 6))
      colnames(iter_data) <- c("Treatment1", "Treatment2", "Treatment3", "Treatment4", "Method", "SampleSize")
      
      for (t in 1:4){
        for (m in 1:nrow(subset_iter)){
          iter_data[m,t] <- sum(subset_iter[m,1:4]==t)/4
          iter_data[m,5] <- subset_iter[m,5]
          iter_data[m,6] <- subset_iter[m,6]
    }
    
   
        
  }
  
 
      
      
      alliter_data <- rbind(alliter_data, iter_data) 
      
    }
    
    null_data <-  rbind(null_data, alliter_data) 
    
    
    
  }
  
  
  null_data <- null_data[!is.na(null_data$Treatment1),]
  
  plotting <- null_data 
  
  plotm1 <- plotting[1,]
  
  for(q in n){
    for (m in unique(plotting$Method)){
    sample_meth <- subset(plotting, SampleSize == q & Method == m)
    
    sample_meth[,1] <- mean(sample_meth[,1])
    sample_meth[,2] <- mean(sample_meth[,2])
    sample_meth[,3] <- mean(sample_meth[,3])
    sample_meth[,4] <- mean(sample_meth[,4])
    
    plotm1 <- rbind(plotm1, sample_meth)
    }
  }
  
  plotm1 <- plotm1[!duplicated(plotm1),]
  plotm1 <- plotm1[-1,]
  plotm1 <- reshape2::melt(plotm1, id.var = c("Method", "SampleSize"))
  #plotm1 <- plotm1[!duplicated(plotm1),]
  
 # plotm1 <- plotting
  #plotm1 <- plotting[plotting$Method == meth,]
 # plotm1 <- plotm1[!duplicated(plotm1[,1:3]),] ####NOTE TO SELF: do we use best_tx which always chooses a best, or plot t1 errors only (ie, chose best when no best)
  #+ facet_grid(. ~ Method) removed 
 
  fm <- ggplot(plotm1, aes(x = SampleSize, y = value, fill = variable)) +
    geom_col() + scale_x_continuous(breaks = n)+ facet_grid(. ~ Method)+ylab("Proportion of Bests") + xlab("Sample Size")+labs(fill = NULL)+theme_minimal()+
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
      panel.grid.major.x = element_blank(), axis.text.x = element_blank(), strip.text = element_text(size = 8)
    )+ 
    guides(color = guide_legend(ncol = 2), 
           shape = guide_legend(ncol = 2)) #+ ggtitle()
  
  return(fm)
}