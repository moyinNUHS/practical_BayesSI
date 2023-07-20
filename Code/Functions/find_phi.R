## function to find phi - which is the effect of a particular treatment

find_phi <- function(p, alpha) {
  log(p / (1 - p)) - alpha
}
