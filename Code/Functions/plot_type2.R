# plot type 1 error - for non-NULL scenarios only

plot_type2 <- function (scenario, d) {
  
  if (scenario == '1.2') {
    best_tx = 'treatment2'
  } else if (scenario %in% c('1.3', '1.4', '2.2', '2.4', '3.2', '4.1', '4.2', '4.3') ){
    best_tx = 'treatment1'
    secbest_tx = 'treatment2'
  } else {
    message('Check scenario label. It should be input as x.x')
  }
  
  # make a long form data
  n = extract_numeric(names(d))
  long_raw = list()
  for (i in n) {
    subset_size = d[[grep(as.character(i), names(d))]]
    raw = as.data.frame(do.call(rbind, subset_size$analyse_out$contiguous_grp))
    raw$method = rep(names(subset_size$analyse_out$contiguous_grp), each = length(subset_size$scenario_out))
    raw$n = i
    long_raw[[i]] = raw
  }
  long = as.data.frame(do.call(rbind, long_raw))
  
  # calculate type 2 error 
  
  
  
  return()
}