# performance of each method
ex_performance <- function(q, k, output_replication, R) {
  mat_all <- do.call(cbind, lapply(1:R, function(z) {
    output_replication[[z]]$performance_m[[q]][, k]
  }))
  apply(mat_all, 1, function(x)
    mean(x, na.rm = T))
}