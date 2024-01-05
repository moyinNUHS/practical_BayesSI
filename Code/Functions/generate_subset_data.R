# ------------------------------- #
# generate data for each subgroup #
# ------------------------------- #

generate_subset_data<-function(i, 
                               size_pattern., 
                               pattern., 
                               res_probability_all., 
                               res_probability_all_site., 
                               differsite = 0){
  pattern_s<-pattern.[[i]]
  sp<-size_pattern.[i]
  res_p<-res_probability_all.[i, pattern_s]
  
  assigned_treatment<-t(rmultinom(sp, 1, 
                                  rep(1/length(pattern_s), length(pattern_s))))
  colnames(assigned_treatment)<-paste0("t",pattern_s)
  
  treatment_label<-apply(assigned_treatment, 1, 
                         function(x){ 
                           x[which(x==max(x))]<-pattern_s[which(x==max(x))]
                           } )

                         
  responses<-lapply(1:length(pattern_s), 
                  function(j){
                    rbinom(sum(treatment_label==pattern_s[j]), 1, res_p[j])
                    })
                    
  sorted_treatment_label<-unlist(lapply(1:length(pattern_s),
                                   function(j){
                                     rep(pattern_s[j],length(responses[[j]]))
                                     }))
  
  pattern_lab<-rep(i,  sp)
  responses<-unlist(responses)

  site_label <- rep(1, sp) 
  ##all sites are the same unless there are different effects specified with differsite 

  treat.site.comb<-cbind(responses=responses, 
                       treatment_label=sorted_treatment_label, 
                       pattern_lab=pattern_lab,
                       site_label = site_label)
  treat.site.comb<-as.data.frame(treat.site.comb)
  treat.site.comb$treatment_label<-as.numeric(treat.site.comb$treatment_label)
  treat.site.comb$site_label<-as.numeric(treat.site.comb$site_label)
  
  if (differsite > 0){
   # assigned_site<-t(rmultinom(sp, 1, 
    #                       rep(1/10, 10)))     
  
    treamentfreq = table(treat.site.comb$treatment_label)
    assigned_site <- NULL
    for (w in 1:length(treamentfreq)){
      assigned_site<-rbind(assigned_site, 
                       t(rmultinom(treamentfreq[w], 1, 
                                   rep(1/10, 10)))
      )
    }
    
    site_s = 1:10
  
    site_label<-apply(assigned_site, 1, 
                  function(x) x[which(x==max(x))]<-site_s [which(x==max(x))] )
    
    treat.site.comb$site_label<-as.numeric(site_label) ##to rewrite site labels within dependency 
                      
    res_p1 = res_probability_all_site.[i, pattern_s]
    for (j in 1:length(pattern_s)){
      treat.site.comb$responses[treat.site.comb$treatment_label==pattern_s[j] & treat.site.comb$site_label<= differsite ] = 
        rbinom(1, 1, res_p1[j])
    }
  }
                                    
  return(list(responses=treat.site.comb$responses, 
              treatment_label=treat.site.comb$treatment_label, 
              pattern_lab=treat.site.comb$pattern_lab,
              site_label = treat.site.comb$site_label))
}
