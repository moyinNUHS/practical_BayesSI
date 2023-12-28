# identify the best treatment

identify_bestR <- function(k, identified_best_t, res_probability_all) {
  v <- identified_best_t[, k]
  t.rate <- res_probability_all[k, v]
  return(t.rate)
}
