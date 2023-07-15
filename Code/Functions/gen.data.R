# generate trial data

gen.data <- function(no_pattern, 
                     size_pattern, 
                     pattern, 
                     res_probability_prior, 
                     res_probability_all,
                     differsite){    
  
  # generate one prior dataset - to use as priors for Bayesian analysis
  Alldata_prior <- sapply(1:no_pattern, function(i){
    generate_subset_data(i, 
                         size_pattern. = size_pattern, 
                         pattern. = pattern, 
                         res_probability_all. = res_probability_prior)
  })
  
  # generate one current trial dataset
  Alldata<-sapply(1:no_pattern, function(i){
    generate_subset_data(i, size_pattern. = size_pattern, 
                         pattern. = pattern, 
                         res_probability_all. = res_probability_all,
                         differsite = differsite)})
  
  # show how many have been randomized to a treatment arm within a pattern
  freq_t_subgroup <- sapply(1:no_pattern, function(i) table(Alldata[2,][[i]]))
  
  # show how many have been randomized to each treatment arm in the overall trial
  freq_t <- table(unlist(Alldata[2,]))
  
  return(list(prior_data = Alldata_prior, 
              trial_data = Alldata, 
              freq_t_subgroup = freq_t_subgroup, 
              freq_t = freq_t))
}
