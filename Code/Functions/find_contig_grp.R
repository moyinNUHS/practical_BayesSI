
find_contig_grp <- function(df) {
  
  # Extract ranges from the data.frames 
  ranges = apply(df, 1, function(tx) {
    c(tx[['LL']], tx[['UL']])
  })
  
  # Create list for comparison 
  rangelab_tb_all = expand.grid(colnames(ranges), colnames(ranges)) # possible comparisons
  rangelab_tb = rangelab_tb_all[which(rangelab_tb_all[,1] != rangelab_tb_all[,2]),] # remove rows with the same comparisons
  rangelab_tb_uniq = unique(t(apply(rangelab_tb, 1, sort))) # remove rows with the same comparisons
  range_tb = t(apply(rangelab_tb_uniq, 1, function(x){
    c(ranges[,x[1]], ranges[,x[2]])
  })) # a table where first column is lower limit of first treatment effect to be compared, 
  #                   second column is upper limit of first treatment effect to be compared, 
  #                   third column is lower limit of second treatment effect to be compared,
  #                   last column is upper limit of second treatment effect to be compared.
  
  # Function to check if two ranges overlap
  check_overlap <- function(x) {
    if(any(is.na(x))) {
      
      return('no estimates')
      
    } else {
      if (x[1] <= x[4] && x[2] >= x[3]) {
        
        return('overlap')  # Ranges overlap
        
      } else {
        
        if (x[2] < x[3]) {
          
          return('1st is better') # Ranges do not overlap
          
        } else {
          
          return('2nd is better') # Ranges do not overlap
          
        }
        
      }
    }
  }
  
  out = apply(range_tb, 1, check_overlap)
  names(out) = paste(rangelab_tb_uniq[,1], rangelab_tb_uniq[,2], sep = '-')
  
  # Label which treatment is better 
  out[which(out == '1st is better')] = parse_number(names(out)[which(out == '1st is better')])
  out[which(out == '2nd is better')] = substr((names(out)[which(out == '2nd is better')]), 
                                              nchar((names(out)[which(out == '2nd is better')])), 
                                              nchar((names(out)[which(out == '2nd is better')])))
  
  return(out)
}



