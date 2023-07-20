# identify the suggested treatment

suggested_treatment <- function(q, R, output_replication) {
  
  all_out <-
    do.call(cbind, lapply(1:R, function(z) {
      output_replication[[z]]$identified_best_t[, q]
    }))
  
  
  t(apply(all_out, 1, function(x) {
    if (all(is.na(x))) {
      rep(NA, 3)
    } else{
      quantile(
        x,
        probs = c(0.25, 0.5, 0.75),
        type = 1,
        na.rm = T
      )
    }
  }))
}
