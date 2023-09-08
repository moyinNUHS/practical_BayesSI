# plot type 1 error - for NULL scenarios only

plot_type1 <- function (scenario, d) {
  
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
  
  # calculate type 1 error 
  
  
  
  return()
}