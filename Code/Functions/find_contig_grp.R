
find_contig_grp <- function(df) {
  
  ranges = apply(df, 1, function(x) {
    c(x[['LL']], x[['UL']])
  })
  
  # Create list for comprison 
  rangelab_tb_all = expand.grid(colnames(ranges), colnames(ranges)) # possible comparisons
  rangelab_tb = rangelab_tb_all[which(rangelab_tb_all[,1] != rangelab_tb_all[,2]),] # remove rows with the same comparisons
  rangelab_tb_uniq = unique(t(apply(rangelab_tb, 1, sort))) # remove rows 
  range_tb = t(apply(rangelab_tb_uniq, 1, function(x){
    c(ranges[,x[1]], ranges[,x[2]])
  }))
  
  # Function to check if two ranges overlap
  if(any(is.na(x)) ) {
    out = NA
  }else{
    check_overlap <- function(x) {
      if (x[1] <= x[4] && x[2] >= x[3]) {
        return(TRUE)  # Ranges overlap
      } else {
        return(FALSE) # Ranges do not overlap
      }
    }
    out = apply(range_tb, 1, check_overlap)
    names(out) = paste(rangelab_tb_uniq[,1], rangelab_tb_uniq[,2], sep = '-')
  }
  
  return(out)
}



