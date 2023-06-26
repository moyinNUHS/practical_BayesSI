# ---------------------------------------------------- #
# the following is to run simulation with replications #
# ---------------------------------------------------- #

simulation<-function(N, phi_v, pattern, 
                     res_probability_prior, res_probability_all,
                     prob_pattern, R){
  
  no_pattern<<-length(pattern) 
  # number of randomization lists
  
  no_comparison<<-sapply(1:no_pattern, function(i){length(pattern[[i]])-1})
  # for each randomization list, the number of pairwise comparisons fixing a reference treatment. 
  
  no_treatment<<-length(unique(unlist(pattern)))
  # number of treatments
  
  #res_probability_all<-matrix(rep(response_prob_V, no_pattern), ncol = no_treatment, byrow = T)
  colnames(res_probability_all)<-sapply(1:no_treatment, function(i){paste0("treatment_", i)} )
  rownames(res_probability_all)<-sapply(1:no_pattern, function(i){paste0("alpha_", i)} )
  # response rate: row = pattern, column=treatment. All rows have same values for this scenario
  
  
  # each person has prob_pattern to be allocated to one of the treatment patterns
  assigned_pattern<-t(rmultinom(N, size=1, prob_pattern))
  colnames(assigned_pattern)<-sapply(1:no_pattern, function(i){paste0("subgroup", i)} )
  
  # number of patients in each subgroup that is defined by the pattern
  size_pattern<<-apply(assigned_pattern, 2, sum)
  lambda<-prob_pattern # true prevalence rate of patterns
  
  true.response.r<-lapply(1:no_pattern,function(i)res_probability_all[i, pattern[[i]]])
  # response rates of the treatments in each pattern
  
  true.mean.min<-lapply(1:no_pattern, function(i){
    v<-true.response.r[[i]]
    c("mean"=mean(v), "min"=min(v)) } )
  
  true.mean.min<-do.call(cbind, true.mean.min)
  # compute the mean (and minimum value) of the treatments in each pattern 
  # will be used for the performance measures about the treatment decisions 

  #For Bayesian Framework, we must choose weight we wish to assign to prior distirbution.
  #We explore both a weakly & strongly informative prior distribution
  Scale_wk<-5     #Larger scale assigns less importance to prior distribution
  Scale_str<-1    #Smaller scale assigns more importance to prior distribution                    
  
  gen.data<-function(j){    
    
    # generate one prior dataset
    Alldata_prior<-sapply(1:no_pattern, function(i){
      generate_subset_data(i, size_pattern.=size_pattern, 
                           pattern.=pattern, res_probability_all.=res_probability_prior)})
    
    # generate one current dataset
    Alldata<-sapply(1:no_pattern, function(i){
      generate_subset_data(i, size_pattern.=size_pattern, 
                           pattern.=pattern, res_probability_all.=res_probability_all)})
    
    # show how many have been randomized to a treatment arm within a pattern
    feq_t_subgroup<-sapply(1:no_pattern, function(i)table(Alldata[2,][[i]]))
    
    # show how many have been randomized to each treatment arm
    feq_t<-table(unlist(Alldata[2,]))
    
    est_method_C<-fit_onestage_C(Alldata) # use original current data
    est_method_C_NI<-fit_onestage_C_NI(Alldata) # use original current data
    est_method_C_wk<-fit_onestage_C_prior(Alldata_prior, Alldata, Scale_wk) # use original current data +original prior data
    est_method_C_str<-fit_onestage_C_prior(Alldata_prior, Alldata, Scale_str) # use original current data +original prior data
    
    #Use a hierarchical structure
    est_method_C2<-fit_onestage_C_hier(Alldata) # use original current data
    est_method_C2_NI<-fit_onestage_C_hier_NI(Alldata) # use original current data
    est_method_C2_wk<-fit_onestage_C_hier_prior(Alldata_prior, Alldata, Scale_wk) # use original current data +original prior data
    est_method_C2_str<-fit_onestage_C_hier_prior(Alldata_prior, Alldata, Scale_str) # use original current data +original prior data
    
    
    # combine estimated best treatments from all methods, row= methods, column= pattern
    identified_best_t<-rbind(method_C=est_method_C$ranking[1,],
                             method_C_NI=est_method_C_NI$ranking[1,],
                             method_C_wk=est_method_C_wk$ranking[1,],
                             method_C_str=est_method_C_str$ranking[1,],
                             method_C2=est_method_C2$ranking[1,],
                             method_C2_NI=est_method_C2_NI$ranking[1,],
                             method_C2_wk=est_method_C2_wk$ranking[1,],
                             method_C2_str=est_method_C2_str$ranking[1,] 
                             )
    
    n_method<-dim(identified_best_t)[1]
    
    identify_bestR<-function(k){
      #v<-sapply(1:6, function(m){ which( pattern [[k]]==identified_best_t[m,k]) } ) 
      #v<-sapply(1:6, function(m){ 
      #  o1<-which( pattern [[k]]==identified_best_t[m,k])
      #  if(length(o1)==0){NA}else{o1}
      #} )
      
      #t.rate<-true.response.r[[k]]
      #t.rate<-t.rate[v]
      #names(t.rate)<-rownames(identified_best_t)
      
      v<-identified_best_t[,k]
      t.rate<-res_probability_all[k,v]
      return(t.rate)
    }
    
    # true response rate of the estimated best treatment for each pattern(column) from each method (row)
    identify_best_rate<-sapply(1:no_pattern,identify_bestR)
    
    # compute mortality reduction for each pattern(column) from each method (row)
    mortality_gain<-t(sapply(1:n_method, function(m){identify_best_rate[m,]-true.mean.min[1,] }) )
    better_treatment_I<-mortality_gain<0

    # compute maximum possible mortality reduction for each pattern(column) from each method (row) if true best treatment known
    mortality_gain_max<-true.mean.min[2,]-true.mean.min[1,]
    
    # compute maximum possible mortality reduction ratio of each method vs if true best known
    mortality_gain_ratio<-t(sapply(1:n_method, function(m){mortality_gain[m,]/mortality_gain_max }) )
     
    
    diff_min<-t(sapply(1:n_method, function(m){ identify_best_rate[m,]-true.mean.min[2,]  }) )
    
    best_treatment_I<-diff_min==0
    
    nearbest_treatment_5<-diff_min-0.05 <= 0
    nearbest_treatment_10<-diff_min-0.1 <= 0
    
    rownames(mortality_gain)<-rownames(mortality_gain_ratio)<-rownames(better_treatment_I)<-rownames(identified_best_t)
    rownames(diff_min)<-rownames(best_treatment_I)<-rownames(identified_best_t)
    
    rownames(nearbest_treatment_5)<-rownames(nearbest_treatment_10)<-rownames(identified_best_t)
    
    estimand2<-list(mortality_gain=mortality_gain,
                    mortality_gain_ratio=mortality_gain_ratio,
                    better_treatment_I=better_treatment_I,
                    best_treatment_I=best_treatment_I,
                    nearbest_treatment_5=nearbest_treatment_5,
                    nearbest_treatment_10=nearbest_treatment_10,
                    diff_min=diff_min )
    #names(measure)<-c("pattern1", "pattern2", "pattern3", "pattern4", "pattern5", "pattern6", "pattern7", "pattern8")
    
    
    identify_fail<-rbind(method_C=est_method_C$ranking[2,],
                         method_C_NI=est_method_C_NI$ranking[2,],
                         method_C_wk=est_method_C_wk$ranking[2,],
                         method_C_str=est_method_C_str$ranking[2,],
                         method_C2=est_method_C2$ranking[2,],
                         method_C2_NI=est_method_C2_NI$ranking[2,],
                         method_C2_wk=est_method_C2_wk$ranking[2,],
                         method_C2_str=est_method_C2_str$ranking[2,] )
    
    
    list(identified_best_t=identified_best_t,
         est_method_C=est_method_C$contrast.est,
         est_method_C_NI=est_method_C_NI$contrast.est,
         est_method_C_wk=est_method_C_wk$contrast.est,
         est_method_C_str=est_method_C_str$contrast.est,
         est_method_C2=est_method_C2$contrast.est,
         est_method_C2_NI=est_method_C2_NI$contrast.est,
         est_method_C2_wk=est_method_C2_wk$contrast.est,
         est_method_C2_str=est_method_C2_str$contrast.est,
         performance_m=estimand2,
         identify_fail=identify_fail, 
         feq_t_subgroup=feq_t_subgroup, feq_t=feq_t)
    
  }
  
  output_replication<-lapply(1:R, function(k){
    print(paste0('running iteration..', k))
    gen.data(k)})
  
  methodA_fail_no<-rbind(Method_C=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[1,k]}))} ),
                         Method_C_NI=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[2,k]}))} ),
                         Method_C_wk=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[3,k]}))} ),
                         Method_C_str=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[4,k]}))} ),
                         Method_C2=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[5,k]}))} ),
                         Method_C2_NI=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[6,k]}))} ),
                         Method_C2_wk=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[7,k]}))} ),
                         Method_C2_str=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[8,k]}))} ) )
  
  estimator_method_C<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C[,1]}))
  estimator_method_C_NI<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C_NI[,1]}))
  estimator_method_C_wk<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C_wk[,1]}))
  estimator_method_C_str<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C_str[,1]}))
  estimator_method_C2<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2[,1]}))
  estimator_method_C2_NI<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2_NI[,1]}))
  estimator_method_C2_wk<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2_wk[,1]}))
  estimator_method_C2_str<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2_str[,1]}))
  estimator_all<-list(estimator_method_C=estimator_method_C, estimator_method_C_NI=estimator_method_C_NI, 
                      estimator_method_C_wk=estimator_method_C_wk, estimator_method_C_str=estimator_method_C_str,
                      estimator_method_C2=estimator_method_C2, estimator_method_C2_NI=estimator_method_C2_NI, 
                      estimator_method_C2_wk=estimator_method_C2_wk, estimator_method_C2_str=estimator_method_C2_str)
  
  model_var_method_C<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C[,2]}))
  model_var_method_C_NI<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C_NI[,2]}))
  model_var_method_C_wk<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C_wk[,2]}))
  model_var_method_C_str<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C_str[,2]}))
  model_var_method_C2<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2[,2]}))
  model_var_method_C2_NI<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2_NI[,2]}))
  model_var_method_C2_wk<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2_wk[,2]}))
  model_var_method_C2_str<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2_str[,2]}))
  model_var_all<-list(model_var_method_C=model_var_method_C, model_var_method_C_NI=model_var_method_C_NI, 
                      model_var_method_C_wk=model_var_method_C_wk, model_var_method_C_str=model_var_method_C_str, 
                      model_var_method_C2=model_var_method_C2, model_var_method_C2_NI=model_var_method_C2_NI, 
                      model_var_method_C2_wk=model_var_method_C2_wk, model_var_method_C2_str=model_var_method_C2_str)
  
  # compute the property of estimator
  com_property<-function(out_one, q){
    if(all( is.na(out_one[,1]) ) ){rep(NA,n_method)}else{
      val<- out_one[,1]
      val<- as.numeric(val[complete.cases(val)])
      t.diff<-(phi_v[q+1]-phi_v[1])
      bias<-mean(val-t.diff); var.s<-var(val)
      
      meanv2<-mean(as.numeric(out_one[,2]), na.rm = T)
      v2<-out_one[,-c(1,3)]
      #meanv2<-apply(v2, 2, function(x){
      #  if(all(is.numeric(x))){mean(x)}else{
      #    indx<-which(is.na(str_extract(x, "[0-9]+")))
      #    mean(as.numeric(x[-indx])) } }  )
      
      pw<-v2[which(is.na(str_extract(v2[,3], "[0-9]+"))), 3]
      
      
      coverage_ind<-rbind(out_one[,5] >=t.diff, out_one[,4] <=t.diff)
      coverage_count<-apply(coverage_ind, 2, sum)
      coverage_prob<-length(which(coverage_count==2)) / length(which(is.na(coverage_count)==F))
      
      MSE<-mean((val-t.diff)^2)
      MCSE_mse<-sqrt( var((val-t.diff)^2)/R )
      list(pw, c(bias=bias, empirical_var=var.s, coverage_prob=coverage_prob, mse=MSE, 
                 MCSE_bias=sqrt(var.s/R), 
                 MCSE_cov_p=sqrt(coverage_prob*(1-coverage_prob)/R), 
                 MCSE_MSE=MCSE_mse,
                 ex_model_var=meanv2,  
                 fail.no=length(which(is.na(out_one[,1])))  ) )
    }
  }

  estimator_prop<-function(q){ #q=1; out_one=method_C
    method_C<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C[q,]}))
    method_C_NI<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C_NI[q,]}))
    method_C_wk<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C_wk[q,]}))
    method_C_str<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C_str[q,]}))
    method_C2<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2[q,]}))
    method_C2_NI<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2_NI[q,]}))
    method_C2_wk<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2_wk[q,]}))
    method_C2_str<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C2_str[q,]}))
    method_Co=com_property(method_C, q)
    method_Co_NI=com_property(method_C_NI, q)
    method_Co_wk=com_property(method_C_wk, q)
    method_Co_str=com_property(method_C_str, q)
    method_Co_2=com_property(method_C2, q)
    method_Co_2_NI=com_property(method_C2_NI, q)
    method_Co_2_wk=com_property(method_C2_wk, q)
    method_Co_2_str=com_property(method_C2_str, q)
    list( method_C_warning=method_Co[[1]], method_C_NI_warning=method_Co_NI[[1]],
          method_C_wk_warning=method_Co_wk[[1]], method_C_str_warning=method_Co_str[[1]], 
          method_C2_warning=method_Co_2[[1]], method_C2_NI_warning=method_Co_2_NI[[1]],
          method_C2_wk_warning=method_Co_2_wk[[1]], method_C2_str_warning=method_Co_2_str[[1]],
          property=cbind(method_Cp=method_Co[[2]], method_Cp_NI=method_Co_NI[[2]], 
                         method_Cp_wk=method_Co_wk[[2]], method_Cp_str=method_Co_str[[2]],
                         method_Cp_2=method_Co_2[[2]], method_Cp_2_NI=method_Co_2_NI[[2]], 
                         method_Cp_2_wk=method_Co_2_wk[[2]], method_Cp_2_str=method_Co_2_str[[2]] ) )
    
    
  }
  
  estimator_property<-lapply(1:(no_treatment-1),estimator_prop)
  names(estimator_property)<-sapply(2:no_treatment, function(i)paste0("phi",i))
  method_C_property<-sapply(1:(no_treatment-1), function(i)estimator_property[[i]]$property[,1])
  method_C_NI_property<-sapply(1:(no_treatment-1), function(i)estimator_property[[i]]$property[,2])
  method_C_wk_property<-sapply(1:(no_treatment-1), function(i)estimator_property[[i]]$property[,3])
  method_C_str_property<-sapply(1:(no_treatment-1), function(i)estimator_property[[i]]$property[,4])
  method_C2_property<-sapply(1:(no_treatment-1), function(i)estimator_property[[i]]$property[,5])
  method_C2_NI_property<-sapply(1:(no_treatment-1), function(i)estimator_property[[i]]$property[,6])
  method_C2_wk_property<-sapply(1:(no_treatment-1), function(i)estimator_property[[i]]$property[,7])
  method_C2_str_property<-sapply(1:(no_treatment-1), function(i)estimator_property[[i]]$property[,8])
  
  method_c_property<-t(method_C_property)#[,c(1,2,5,6)]
  method_c_NI_property<-t(method_C_NI_property)#[,c(1,2,5,6)]
  method_c_wk_property<-t(method_C_wk_property)#[,c(1,2,5,6)]
  method_c_str_property<-t(method_C_str_property)#[,c(1,2,5,6)]
  method_c2_property<-t(method_C2_property)#[,c(1,2,5,6)]
  method_c2_NI_property<-t(method_C2_NI_property)#[,c(1,2,5,6)]
  method_c2_wk_property<-t(method_C2_wk_property)#[,c(1,2,5,6)]
  method_c2_str_property<-t(method_C2_str_property)#[,c(1,2,5,6)]
  
  # identify the suggested treatment
  suggested_treatment<-function(q){
    all_out<-do.call(cbind,lapply(1:R, function(z){output_replication[[z]]$identified_best_t[,q]}))
    t(apply(all_out, 1,function(x){
      if(all(is.na(x))){rep(NA,3)}else{
        quantile(x, probs =c(0.25,0.5,0.75), type = 1, na.rm = T ) } } ) )
  }
  
  suggested_treatment_each<-lapply(1:no_pattern, suggested_treatment)
  names(suggested_treatment_each)<-sapply(1:no_pattern, function(i)paste0("pattern",i))
  
  
  # performance of each method
  ex_performance<-function(q,k){
    mat_all<-do.call(cbind,lapply(1:R, function(z){
      output_replication[[z]]$performance_m[[q]][,k] } ) )
    apply(mat_all, 1, function(x)mean(x, na.rm = T))
  }
  
  ex_performance_out<-lapply(names(output_replication[[1]]$performance_m)[-6],
                             function(j)sapply(1:no_pattern, function(i)ex_performance(j,i) ) )
  names(ex_performance_out)<-names(output_replication[[1]]$performance_m)[-6]
  
  estimand2<-do.call(cbind,lapply(ex_performance_out, function(x){apply(x,1, function(y){sum(y*lambda)})}) )
  
  estimand2_MCSE<-sqrt(estimand2[,3:6]*(1-estimand2[,3:6])/R )
  
  all_diff_min<-lapply(1:R, function(z){output_replication[[z]]$performance_m$diff_min })  
  
  mortality_gain<-do.call(rbind, lapply(1:R, function(z){
    apply(output_replication[[z]]$performance_m$mortality_gain, 1, function(x){sum(x*lambda)}) } )   )
  
  mortality_gain_ratio<-do.call(rbind, lapply(1:R, function(z){
    apply(output_replication[[z]]$performance_m$mortality_gain_ratio, 1, function(x){sum(x*lambda)}) } )   )
  
  estimand2_MCSE<-cbind(Mortaliy_ratio=apply(mortality_gain_ratio, 2,function(x){ 
    sqrt(var(x[complete.cases(x)])/length(x[complete.cases(x)])) }), estimand2_MCSE)
  
  estimand2_MCSE<-cbind(Mortaliy=apply(mortality_gain, 2,function(x){ 
    sqrt(var(x[complete.cases(x)])/length(x[complete.cases(x)])) }), estimand2_MCSE)
  
  size_per_arm<-size_pattern/(no_comparison+1)
  
  each_t<-function(k){
    
    com_size_each<-function(i){
      
      v<-pattern[[i]]
      m<-cbind(v,rep(size_per_arm[i],length(v)) )
      
      #m<-size_per_gp[[i]]
      
      tv<-which(m[,1]==k)
      if(length(tv)==0){ 0 }else{ m[tv,2]}
      
    }
    sum(sapply(1:length(pattern), com_size_each))
  }
  
  ex_arm_size<-sapply(1:no_treatment, each_t)
  
  
  t_feq<-t(sapply(1:R, function(r)output_replication[[r]]$feq_t))
  feq_treatment<-apply(t_feq, 2, summary)[c(1,3,4,6), ]
  
  
  out<-list(method_C_property=method_c_property,
            method_C_NI_property=method_c_NI_property,
            method_C_wk_property=method_c_wk_property,
            method_C_str_property=method_c_str_property,
            method_C2_property=method_c2_property,
            method_C2_NI_property=method_c2_NI_property,
            method_C2_wk_property=method_c2_wk_property,
            method_C2_str_property=method_c2_str_property,
            ex_performance_out=ex_performance_out,
            suggested_treatment_each=suggested_treatment_each,
            estimator_all=estimator_all,
            #model_var_all=model_var_all,
            #all_diff_min=all_diff_min,
            method_fail_no=methodA_fail_no,
            estimand2=estimand2,
            estimand2_MCSE=estimand2_MCSE, 
            ex_arm_size=ex_arm_size,
            overall_size=feq_treatment, 
            Pattern=pattern)
  return(out)
}
