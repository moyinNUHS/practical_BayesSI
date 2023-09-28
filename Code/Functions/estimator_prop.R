estimator_prop <- function(q, output_replication, method.names, T_v, R,scenario_name) { # q being the selected treatment
  
  out = list()
  n_method = length(method.names) # n_method is the number of methods
  # get a list of estimator properties for each method 
  for (m in method.names){
    df_list = map(output_replication, m)
    rows = lapply(df_list, function(x) { x[q,] })
    com = do.call(rbind, rows)
    out[[m]] = com_property(com, q, n_method, T_v, R,scenario_name)
  }
  
  warning = lapply(out, `[[`, 1)
  property = do.call(cbind, lapply(out, `[[`, 2))
  
  list(warning = warning, property = property)

}
