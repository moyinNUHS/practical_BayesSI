# ------------------------------- #
# generate data for each subgroup #
# ------------------------------- #

generate_subset_data<-function(k, size_pattern., pattern., res_probability_all., differsite = 0){
  # size_pattern.=size_pattern;   pattern.=pattern;  res_probability_all.=res_probability_all
  pattern_s<-pattern.[[k]]
  sp<-size_pattern.[k]
  res_p<-res_probability_all.[k, pattern_s]
  
  assigned_treatment<-t(rmultinom(sp, 1, 
                                  rep(1/length(pattern_s), length(pattern_s))))
  
  colnames(assigned_treatment)<-paste0("t",pattern_s)
  
  treatment_label<-apply(assigned_treatment, 1, 
                         function(x) x[which(x==max(x))]<-pattern_s[which(x==max(x))] )

assigned_site<-t(rmultinom(sp, 1, 
                           rep(1/10, 10)))
site_s = 1:10
site_label<-apply(assigned_site, 1, 
                  function(x) x[which(x==max(x))]<-site_s [which(x==max(x))] )
                         
  responses<-lapply(1:length(pattern_s), 
                    function(j)rbinom(sum(treatment_label==pattern_s[j]), 1, res_p[j]))

if (differsite > 0){
  res_p1 = res_p[length(res_p):1]
for (j in 1:length(pattern_s)){
  responses[[j]][site_label[treatment_label==pattern_s[j]] <= differsite ] = 
    rbinom(sum(treatment_label==pattern_s[j]&site_label <= differsite), 1, res_p1[j])
}
}

                    
  assigned_treatment<-unlist(lapply(1:length(pattern_s),function(j)rep(pattern_s[j],length(responses[[j]])) ))
  
  assigned_pairwise<-unlist(lapply(1:length(pattern_s),function(j){rep(pattern_s[-j], sum(treatment_label==pattern_s[j])) } ) )
  
  rep_assigned_t<-rep(assigned_treatment, each=length(pattern_s)-1)
  assigned_pairwise<-paste(rep_assigned_t, assigned_pairwise, sep="_")
  
  pattern_lab<-rep(k,  sp)#unlist(sapply(1:no_pattern, function(j)rep(j,  sp)) )
  responses<-unlist(responses)
  return(list(responses=responses, 
              treatment_label=assigned_treatment, 
              assigned_pairwise=assigned_pairwise, pattern_lab=pattern_lab,
             site_label = site_label))
}
