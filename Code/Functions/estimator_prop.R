

estimator_prop <- function(q, output_replication, method.names, phi_v, R) { # q being the comparison treatment with reference treatment 
  
  out = list()
  
  # get a list of estimator properties for each method 
  for (m in method.names){
    df_list = map(output_replication, m)
    rows = lapply(df_list, function(x) { x[q,] })
    com = do.call(rbind, rows)
    out[[m]] = com_property(com, q, n_method, phi_v, R)
  }
  
  warning = lapply(out, `[[`, 1)
  property = do.call(cbind, lapply(out, `[[`, 2))
  
  list(warning = warning, property = property)

}