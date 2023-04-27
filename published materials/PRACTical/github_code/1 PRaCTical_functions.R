# 04/10/2021 created by Kim M Lee, King's College London
# This script contains data generating function and analysis methods.
# Please check note 1 for a potential issue when using method D
# on different versions of R

# function "fit_sub" is to be used in methods B1, B2 and B3


rm(list = ls())
library(stringr)
library(multiwayvcov)
require(sandwich)
# -------------------- #
# response probability #
# -------------------- #
res_probability<-function(phi,alpha=-1.36){
  exp(alpha+ phi)/(1+ exp(alpha+ phi) )
}

# logit for finding phi #
find_phi<-function(p, alpha=-1.36){log(p/(1-p)) - alpha } 

# ------------------------------- #
# generate data for each subgroup #
# ------------------------------- #
generate_subset_data<-function(k, size_pattern., pattern., res_probability_all.){
  # size_pattern.=size_pattern;   pattern.=pattern;  res_probability_all.=res_probability_all
  pattern_s<-pattern.[[k]]
  sp<-size_pattern.[k]
  res_p<-res_probability_all.[k, pattern_s]
  
  assigned_treatment<-t(rmultinom(sp, 1, 
                                  rep(1/length(pattern_s), length(pattern_s))))
  
  colnames(assigned_treatment)<-paste0("t",pattern_s)
  
  treatment_label<-apply(assigned_treatment, 1, 
                         function(x) x[which(x==max(x))]<-pattern_s[which(x==max(x))] )
  
  responses<-lapply(1:length(pattern_s), 
                    function(j)rbinom(sum(treatment_label==pattern_s[j]), 1, res_p[j]))
  assigned_treatment<-unlist(lapply(1:length(pattern_s),function(j)rep(pattern_s[j],length(responses[[j]])) ))
  
  assigned_pairwise<-unlist(lapply(1:length(pattern_s),function(j){rep(pattern_s[-j], sum(treatment_label==pattern_s[j])) } ) )
  
  rep_assigned_t<-rep(assigned_treatment, each=length(pattern_s)-1)
  assigned_pairwise<-paste(rep_assigned_t, assigned_pairwise, sep="_")
  
  pattern_lab<-rep(k,  sp)#unlist(sapply(1:no_pattern, function(j)rep(j,  sp)) )
  responses<-unlist(responses)
  return(list(responses=responses, 
              treatment_label=assigned_treatment, 
              assigned_pairwise=assigned_pairwise, pattern_lab=pattern_lab))
}

# -------------------------------------------------------------- #
# identify suggested treatment for a pattern given the contrasts #
# -------------------------------------------------------------- #
find.rankings<-function(t_labelv, treat.coeff){ 
  #treat.coeff has size=no_treatment
  #t_labelv=t_label; treat.coeff=est.contrasts
  
  if(all(is.na(treat.coeff)) ){
    # if all est.contrasts are NA
    out<-sample(t_labelv,1) # random recommendation
    not_fit<-1 # record model not fitted
  }else{
    out<-which.min(treat.coeff)
    not_fit<-0
  }
  
  return(c(out, not_fit))
}



# -------------------------------------- #
# method_A: fit a model to each subgroup # 
# -------------------------------------- #


fit_subgroup_A<-function(alldata){
  no_p<-no_pattern
  
  fit_subA<-function(i){ # i corresponds to pattern
    sub_data<-alldata[,i]
    sub_data$treatment<-factor(sub_data$treatment_label, levels = sort(unique(sub_data$treatment_label)))
    my.glm<-myTryCatch(glm(responses~ treatment, data=sub_data,family="binomial"))
    t_label<-sort(unique(sub_data$treatment_label)) 
    
    if(is.null(my.glm$error) ) # if there is no error in model fit (there could be warning)
    { 
      sub_fit<-my.glm[[1]]
      
      fit.coeff<-coef(sub_fit)[2:length(t_label)]
      fit.coeff[which(abs(fit.coeff)>12)]<-NA
      est.contrasts<-rep(NA, no_treatment)
      if(all(is.na(fit.coeff))){
        est.contrasts<-est.contrasts
      }else{
        est.contrasts[t_label[1]]<-0
        est.contrasts[t_label[-1]]<-fit.coeff}
    }else{ est.contrasts<-rep(NA, no_treatment) }
    
    fr<-find.rankings(t_labelv=t_label, treat.coeff=est.contrasts)
    
    return(list(out=est.contrasts, rankings.out=fr ) )
      
  }
    
  out<-sapply(1:no_p, fit_subA)
  rank.v<- do.call("cbind", out[2,])
  colnames(rank.v)<-sapply(1:no_pattern, function(i)paste("pattern", i))
  row.names(rank.v)<-c("suggested treatment", "model.not.fit")
  return(list(contrast.est= do.call("cbind", out[1,]),
              ranking= rank.v) )
}



# -------------------------------------------------------#
#     For methods B. function to fit a model to data     #
# adjust for pattern if using the data of other patterns #
# ------------------------------------------------------ #
fit_sub<-function(sub_data){
  #sub_data<-data1 is a matrix of data, #sub_data[,1]= response  #sub_data[,2]= treatment  #sub_data[,3]= pattern
  response<-sub_data[,1]
  treatment_ind<-factor(sub_data[,2], levels = sort(unique(sub_data[,2]))) 
  pattern_var<-sub_data[,3]
  t_label<-sort(unique(sub_data[,2])) 


  if(length(unique(pattern_var))==1){ # there is only one pattern
    
    my.glm<-myTryCatch( glm(response~ treatment_ind,family="binomial"  ) )
    
  }else{ # if there are more than one pattern
    
    my.glm<-myTryCatch( glm(response~ treatment_ind+ pattern_var, family = "binomial" ) )
    
  }
  
  
  if(is.null(my.glm$error) ) # if there is no error in model fit (there could be warning)
  { 
    sub_fit<-my.glm[[1]]
    fit.coeff<-coef(sub_fit)[2:length(t_label)]
    fit.coeff[which(abs(fit.coeff)>12)]<-NA
    est.contrasts<-rep(NA, no_treatment)
    if(all(is.na(fit.coeff))){
      est.contrasts<-est.contrasts
    }else{
      est.contrasts[t_label[1]]<-0
      est.contrasts[t_label[-1]]<-fit.coeff}

  }else{ est.contrasts<-rep(NA, no_treatment) }
  
  fr<-find.rankings(t_labelv=t_label, treat.coeff=est.contrasts)
  
  return(list(out=est.contrasts, rankings.out=fr ) )
  
}

# ------------------------------------ #
# method B1 must include all treatment #
# ------------------------------------ #
methodB1<-function(alldata=Alldata){
  
  no_p<-no_pattern 
  size_p<-size_pattern
  pat<-pattern
  # generate individual data for method B
  id_v<-cumsum(size_p)
  ID<-c(1:id_v[1], unlist(sapply(1:(no_p-1),function(i){ (id_v[i]+1):id_v[i+1] } )) )
  ind_Data<-data.frame(y=unlist(alldata[1,]), treatment=unlist(alldata[2,]), pat_lab=unlist(alldata[4,]),ID=ID)
  patternI<-1:no_p
  
  # identify data that have same treatment to that of pattern k
  extract_f1<-function(j,k){which(ind_Data$treatment==pat[[k]][j]) }
  extract_f2<-function(K){unlist(sapply(1:length(pat[[K]]), function(J)extract_f1(J,K)))}
  
  must_include<-function(i){
    exD<-ind_Data[extract_f2(i),]
    # identify if pattern k has treatment of pattern i, all true means contain all
    contain_t_f<-function(k)sapply(1:length(pat[[i]]), function(j){ length(which(pat[[k]]==pat[[i]][j]))==1 } )
    
    test_o<-sapply(patternI[-i], function(K){if( mean(contain_t_f(K))==1){K}else{NA} })
    incI<-c(i, test_o)
    incI<-incI[complete.cases(incI)]
    inc_ind<-unlist(sapply(incI, function(x) which(exD$pat_lab== x) ) )
    data1<-exD[inc_ind,]
    
    data1$pat_lab<-factor(data1$pat_lab, levels=incI )
    
    fit_sub(data1)
    
  }
  
  out_B1=sapply(1:no_p, must_include)
  
  B1_rank.v<- do.call("cbind", out_B1[2,])
  colnames(B1_rank.v)<-sapply(1:no_pattern, function(i)paste("pattern", i))
  row.names(B1_rank.v)<-c("suggested treatment", "model.not.fit")
  return(list(contrast.est= do.call("cbind", out_B1[1,]),
              ranking= B1_rank.v) )
  
}



# ------------------------------------------------------------------- #
# method B2 minimal two treatments (must contain the first treatment) #
# ------------------------------------------------------------------- #
methodB2<-function(alldata=Alldata){
  
  no_p<-no_pattern 
  size_p<-size_pattern
  pat<-pattern
  
  # generate individual data for method B
  id_v<-cumsum(size_p)
  ID<-c(1:id_v[1], unlist(sapply(1:(no_p-1),function(i){ (id_v[i]+1):id_v[i+1] } )) )
  ind_Data<-data.frame(y=unlist(alldata[1,]), treatment=unlist(alldata[2,]), pat_lab=unlist(alldata[4,]),ID=ID)
  patternI<-1:no_p
  
  # identify data that have same treatment to that of pattern k
  extract_f1<-function(j,k){which(ind_Data$treatment==pat[[k]][j]) }
  extract_f2<-function(K){unlist(sapply(1:length(pat[[K]]), function(J)extract_f1(J,K)))}
  
  minimal<-function(i){
    exD<-ind_Data[extract_f2(i),]
    
    # minimal
    test_first_e<-function(k){ if( length(which(pat[[k]]==pat[[i]][1])) ==1){k}else{NA} }
    incI<-c(i, sapply(patternI[-i],   test_first_e )  )
    incI<-incI[complete.cases(incI)]
    inc_ind<-unlist(sapply(incI, function(x) which(exD$pat_lab== x) ) )
    data1<-exD[inc_ind,]
    # check how many treatments from each pattern
    no.t.per.pattern<-apply( table(data1[,c(2,3)] ), 2, function(x){ length( which(x!=0) ) } )
    
    patl<-colnames( table(data1[,c(2,3)] ) )
    # which extra pattern(s) contribute to less than 2 treatment 
    extra.p.dis<-patl[which(no.t.per.pattern<2)]
    
    # find the corresponding pattern for excluding them in model fit
    exc.index<-unlist(lapply(extra.p.dis, function(x)which(data1[,3]==x) ))
    
    if( is.null(exc.index) ){
      data1<-data1 }else{ data1<-data1[-exc.index,] }
    
    pat.used<-unique(data1$pat_lab)
    
    data1$pat_lab<-factor(data1$pat_lab, 
                          levels= c(i, pat.used[-which(pat.used==i)]))
    
    fit_sub(data1)

  }
  
  out_B2=sapply(1:no_p, minimal)
  
  B2_rank.v<- do.call("cbind", out_B2[2,])
  colnames(B2_rank.v)<-sapply(1:no_pattern, function(i)paste("pattern", i))
  row.names(B2_rank.v)<-c("suggested treatment", "model.not.fit")
  return(list(contrast.est= do.call("cbind", out_B2[1,]),
              ranking= B2_rank.v) )
  
}



# ------------------------------------------------------ #
# method B3 any number, must include the first treatment #
# ------------------------------------------------------ #
methodB3<-function(alldata=Alldata){
  no_p<-no_pattern
  size_p<-size_pattern
  pat<-pattern
  # generate individual data for method B
  id_v<-cumsum(size_p)
  ID<-c(1:id_v[1], unlist(sapply(1:(no_p-1),function(i){ (id_v[i]+1):id_v[i+1] } )) )
  ind_Data<-data.frame(y=unlist(alldata[1,]), treatment=unlist(alldata[2,]), pat_lab=unlist(alldata[4,]),ID=ID)
  patternI<-1:no_p
  
  # identify data that have same treatment to that of pattern k
  extract_f1<-function(j,k){which(ind_Data$treatment==pat[[k]][j]) }
  extract_f2<-function(K){unlist(sapply(1:length(pat[[K]]), function(J)extract_f1(J,K)))}
  
  all_direct<-function(i){
    data1<-ind_Data[extract_f2(i),]
    # check how many treatments from each pattern
    no.t.per.pattern<-apply( table(data1[,c(2,3)] ), 2, function(x){ length( which(x!=0) ) } )
    patl<-colnames( table(data1[,c(2,3)] ) )
    # which extra pattern(s) contribute to less than 2 treatment 
    extra.p.dis<-patl[which(no.t.per.pattern<2)]
    
    # find the corresponding pattern for excluding them in model fit
    exc.index<-unlist(lapply(extra.p.dis, function(x)which(data1[,3]==x) ))
    
    if( is.null(exc.index) ){
      data1<-data1 }else{ data1<-data1[-exc.index,] }
    
    pat.used<-unique(data1$pat_lab)

    data1$pat_lab<-factor(data1$pat_lab, levels= c(i, pat.used[-which(pat.used==i)]))
    
    fit_sub(data1)
    
  }
  
  out_B3=sapply(1:no_p ,all_direct) 
  
  B3_rank.v<- do.call("cbind", out_B3[2,])
  colnames(B3_rank.v)<-sapply(1:no_pattern, function(i)paste("pattern", i))
  row.names(B3_rank.v)<-c("suggested treatment", "model.not.fit")
  return(list(contrast.est= do.call("cbind", out_B3[1,]),
              ranking= B3_rank.v) )
  
}



# ----------------------------------------- #
# method C: fit one step model to all data  # 
# ----------------------------------------- #
fit_onestage_C<-function(alldata=Alldata){
  no_p<-no_pattern
  
  nma_data<-data.frame(y=unlist(alldata[1,]),
                       treatment=factor(unlist(alldata[2,]), levels = sort(unique(unlist(alldata[2,])))),
                       subgroup=factor(unlist(alldata[4,])))#patient_subgroup)))

  my.glm<-myTryCatch(glm(y~treatment+ subgroup,family="binomial",data=nma_data) )
  
  q.val<-qnorm(0.975) 
  
  if(is.null(my.glm$error) ) #if do not have an error, model is fitted
  { 
    my.glm<-my.glm[[1]]
    mof<-summary(my.glm)
    std.err<-sqrt(diag(vcov(mof))[2:no_treatment]) 
    out<-cbind(Estimate=coefficients(mof)[2:no_treatment],
               model_var=std.err^2,
               z=coefficients(mof)[2:no_treatment]/std.err,
               LL=coefficients(mof)[2:no_treatment] - q.val  * std.err,
               UL=coefficients(mof)[2:no_treatment] + q.val  * std.err)
    out[which(abs(out[,1])>12),]<-NA #parameter not converged is set to NA 
    
  }else
  { # if there is error, do not fit model
    out<-matrix(rep(NA,(no_treatment-1)*5),nrow = no_treatment-1, ncol = 5 )
    out[1,5]<-my.glm$error[1]$message
    
  } 
  
  
  # for each subgroup, prepare the coefficients to identify rankings
  prep.coeff<-function(i){
    sub_data<-alldata[,i]
    t_label<-sort(unique(sub_data$treatment_label)) 
    
    if(is.null(my.glm$error) ) # if there is no error in model fit (there could be warning)
    { 

      fit.coeff<-c(0, out[,1])      
      est.contrasts<-rep(NA, no_treatment)
      est.contrasts[t_label]<-fit.coeff[t_label]
      
    }else{ est.contrasts<-rep(NA, no_treatment) }
    
    return(find.rankings(t_labelv=t_label, treat.coeff=est.contrasts) )
    
  }
  
  rank.v<-sapply(1:no_p, prep.coeff)
  
  colnames(rank.v)<-sapply(1:no_pattern, function(i)paste("pattern", i))
  row.names(rank.v)<-c("suggested treatment", "model.not.fit")
  
  return(list(contrast.est=out, ranking=rank.v) )
}



# ---------------------------------------- #
# method D: fit duplicate data in one step # 
# ---------------------------------------- #
fit_robustSE_D<-function(alldata=Alldata){
  #no_com=no_comparison; no_p=no_pattern; size_p=size_pattern
  no_com<-no_comparison
  no_t<-no_treatment
  no_p<-no_pattern
  size_p<-size_pattern
  dup_data<-sapply(1:no_p,function(i){cbind(rep(alldata[1,][[i]], each=no_com[i]),
                                            rep(alldata[2,][[i]], each=no_com[i]),
                                            alldata[3,][[i]] , rep(i, no_com[i]) ) } )
  dup_data<-as.data.frame(do.call(rbind, dup_data),make.names=F )
  colnames(dup_data)<-c("y", "treatment", "pairwise", "pattern")
  dup_data$treatment<-factor(dup_data$treatment, levels = 1:no_treatment)

  # create ID of patients
  id_v<-cumsum(size_p)
  ID<-sapply(1:(no_p-1),function(i){
    ids<-(id_v[i]+1):id_v[i+1]
    rep(ids,each=no_com[1+i])
  })
  ID<-c(rep(1:id_v[1], each=no_com[1]), unlist(ID))
  
  dup_data$id<-ID
  
  
  pairwisev<-sapply(1:no_t,function(i)paste(i, 1:no_t, sep="_"))
  v1=t(pairwisev)[upper.tri(pairwisev)]
  v2=pairwisev[upper.tri(t(pairwisev))]
  
  val<-dup_data$pairwise#val<-paste(dup_data$treatment, dup_data$pairwise, sep="_")
  for(z in 1:(no_t*(no_t-1)/2)){
    test<-which(val==v2[z])
    if(length(test)==0){}else{val[test]<-v1[z]}
  }
  dup_data$id_comparison<-factor(val)
  
  
  # ------------------------- **note 1** ------------------------------------------ #
  # some R version (eg R version 3.6.0 on linux) would convert the class of y to factor, 
  # if so, use the following line instead
  fit_glm<-myTryCatch( glm(as.numeric(levels(y))[y]~treatment+id_comparison,family="binomial",data=dup_data) )
  
  #fit_glm<-myTryCatch( glm(as.numeric(y)~treatment+id_comparison,family="binomial",data=dup_data) )
  
  if(is.null(fit_glm$error) ) #if there is no error, model is fitted 
  { 
    fit_glm<-fit_glm[[1]]
    # Calculate robust standard errors #
    cov.m1 <- cluster.vcov(fit_glm, dup_data$id)[2:no_t, 2:no_t]#vcovHC(fit_glm, type = "HC0")[2:no_t, 2:no_t]
    testcov<-myTryCatch( sqrt(diag(cov.m1)) )
    std.err <- if(is.null(testcov$warning) & is.null(testcov$error) ){testcov$value}else{rep(NaN,no_t-1)} 
    
    q.val <- qnorm(0.975)
    r.est <- cbind( Estimate = coef(fit_glm)[2:no_t], 
                    model_var=std.err^2, #"Robust SE" = std.err, 
                    z = (coef(fit_glm)[2:no_t]/std.err),
                    #"Pr(>|z|) "= 2 * pnorm(abs(coef(fit_glm)[2:no_t]/std.err), lower.tail = FALSE),
                    LL = coef(fit_glm)[2:no_t] - q.val  * std.err,
                    UL = coef(fit_glm)[2:no_t] + q.val  * std.err)

    r.est[which(abs(r.est[,1])>12),]<-NA # unconverged parameter is set to NA
  } else{ # there is error in model fit
    r.est<-matrix(rep(NA,(no_t-1)*5),nrow = no_t-1, ncol = 5 )
    r.est[,5]<-fit_glm$error[1]$message
  }  
  
  # for each subgroup, prepare the coefficients to identify rankings
  prep.coeff<-function(i){
    sub_data<-alldata[,i]
    t_label<-sort(unique(sub_data$treatment_label)) 
    
    if(is.null(fit_glm$error) ) # if there is no error in model fit (there could be warning)
    { 
      
      fit.coeff<-c(0, r.est[,1])      
      est.contrasts<-rep(NA, no_treatment)
      est.contrasts[t_label]<-fit.coeff[t_label]
      
    }else{ est.contrasts<-rep(NA, no_treatment) }
    
    return(find.rankings(t_labelv=t_label, treat.coeff=est.contrasts) )
    
  }
  
  rank.v<-sapply(1:no_p, prep.coeff)
  colnames(rank.v)<-sapply(1:no_pattern, function(i)paste("pattern", i))
  row.names(rank.v)<-c("suggested treatment", "model.not.fit")
  
  
  return(list(contrast.est=r.est, ranking=rank.v) )

}


# ---------------------------------------- #
# to catch warning or error from model fit #
# ---------------------------------------- #
myTryCatch <- function(expr) {
  warn <- err <- NULL
  value <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(value=value, warning=warn, error=err)
}


# ---------------------------------------------------- #
# the following is to run simulation with replications #
# ---------------------------------------------------- #
simulation<-function(N, phi_v, pattern, res_probability_all,
                     prob_pattern,  R=10){
  # N=1000; phi_v=phi_1; pattern=patternV; response_prob_V=res_rate1; prob_pattern=c(0.2, 0.2, rep(0.1, 6)); R=5
  
  no_pattern<<-length(pattern) 
  # number of randomization lists
  
  no_comparison<<-sapply(1:no_pattern, function(i){length(pattern[[i]])-1})
  # for each randomization list, the number of pairwise comparisons fixing a reference treatment. 
  
  no_treatment<<-length(unique(unlist(pattern)))
  # number of treatments
  
  #res_probability_all<-matrix(rep(response_prob_V, no_pattern), ncol = no_treatment, byrow = T)
  colnames(res_probability_all)<-sapply(1:no_treatment, function(i){paste0("treatment_", i)} )
  rownames(res_probability_all)<-sapply(1:no_pattern, function(i){paste0("alpha_", i)} )
  # response rate: row= pattern, column=treatment. All rows have same values for this scenario
  
  
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
  
  gen.data<-function(j){    
    
    # generate one dataset
    Alldata<-sapply(1:no_pattern, function(i){
      generate_subset_data(i, size_pattern.=size_pattern, 
                           pattern.=pattern, res_probability_all.=res_probability_all)})
    
    # show how many have been randomized to a treatment arm within a pattern
    feq_t_subgroup<-sapply(1:no_pattern, function(i)table(Alldata[2,][[i]]))
    
    # show how many have been randomized to each treatment arm
    feq_t<-table(unlist(Alldata[2,]))
    
    est_method_C<-fit_onestage_C(Alldata) # use original data
    est_method_D<-fit_robustSE_D(Alldata) # use duplicated data
    method_A_f<-fit_subgroup_A(Alldata)   # fit each subgroup
    method_B1<-methodB1(alldata = Alldata)
    method_B2<-methodB2(alldata = Alldata)
    method_B3<-methodB3(alldata = Alldata)
    #est_method_C<-method_B2
    #est_method_D<-method_A_f<-method_B1<-est_method_C
    
    
    # combine estimated best treatments from all methods, row= methods, column= pattern
    identified_best_t<-rbind(method_A=method_A_f$ranking[1,],
                             method_B1=method_B1$ranking[1,],
                             method_B2=method_B2$ranking[1,],
                             method_B3=method_B3$ranking[1,],
                             method_C=est_method_C$ranking[1,],
                             method_D=est_method_D$ranking[1,] )
    
    
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
    mortality_gain<-t(sapply(1:6, function(m){identify_best_rate[m,]-true.mean.min[1,] }) )
    better_treatment_I<-mortality_gain<0
    
    
    
    diff_min<-t(sapply(1:6, function(m){ identify_best_rate[m,]-true.mean.min[2,]  }) )
    
    best_treatment_I<-diff_min==0
    
    nearbest_treatment_5<-diff_min-0.05 <= 0
    nearbest_treatment_10<-diff_min-0.1 <= 0
    
    rownames(mortality_gain)<-rownames(better_treatment_I)<-rownames(identified_best_t)
    rownames(diff_min)<-rownames(best_treatment_I)<-rownames(identified_best_t)
    
    rownames(nearbest_treatment_5)<-rownames(nearbest_treatment_10)<-rownames(identified_best_t)
    
    estimand2<-list(mortality_gain=mortality_gain,
                    better_treatment_I=better_treatment_I,
                    best_treatment_I=best_treatment_I,
                    nearbest_treatment_5=nearbest_treatment_5,
                    nearbest_treatment_10=nearbest_treatment_10,
                    diff_min=diff_min )
    #names(measure)<-c("pattern1", "pattern2", "pattern3", "pattern4", "pattern5", "pattern6", "pattern7", "pattern8")
    
    
    identify_fail<-rbind(method_A=method_A_f$ranking[2,],
                         method_B1=method_B1$ranking[2,],
                         method_B2=method_B2$ranking[2,],
                         method_B3=method_B3$ranking[2,],
                         method_C=est_method_C$ranking[2,],
                         method_D=est_method_D$ranking[2,] )
    
    
    list(identified_best_t=identified_best_t,
         est_method_A=method_A_f$contrast.est,
         est_method_B1=method_B1$contrast.est,
         est_method_B2=method_B2$contrast.est,
         est_method_B3=method_B3$contrast.est,
         est_method_C=est_method_C$contrast.est,
         est_method_D=est_method_D$contrast.est,
         performance_m=estimand2,
         identify_fail=identify_fail, 
         feq_t_subgroup=feq_t_subgroup, feq_t=feq_t)
    
  }
  output_replication<-lapply(1:R, function(k){
    print(k)
    gen.data(k)})
  
  methodA_fail_no<-rbind(Method_A=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[1,k]}))} ),
                         Method_B1=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[2,k]}))} ),
                         Method_B2=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[3,k]}))} ),
                         Method_B3=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[4,k]}))} ),
                         Method_C=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[5,k]}))} ),
                         Method_D=sapply(1:no_pattern, function(k){ sum(sapply(1:R, function(z){output_replication[[z]]$identify_fail[6,k]}))} ) )
  
  estimator_method_C<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C[,1]}))
  estimator_method_D<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_D[,1]}))
  estimator_all<-list(estimator_method_C=estimator_method_C, estimator_method_D=estimator_method_D)
  
  model_var_method_C<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_C[,2]}))
  model_var_method_D<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_D[,2]}))
  model_var_all<-list(model_var_method_C=model_var_method_C, model_var_method_D=model_var_method_D)
  
  # compute the property of estimator
  com_property<-function(out_one, q){
    if(all( is.na(out_one[,1]) ) ){rep(NA,6)}else{
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
    method_D<-do.call(rbind, lapply(1:R, function(z){output_replication[[z]]$est_method_D[q,]}))
    method_Co=com_property(method_C, q)
    method_Do=com_property(method_D, q)
    list( method_C_warning=method_Co[[1]],  method_D_warning=method_Do[[1]] ,
          property=cbind(method_Cp=method_Co[[2]],  method_Dp=method_Do[[2]]  ) )
    
    
  }
  
  estimator_property<-lapply(1:(no_treatment-1),estimator_prop)
  names(estimator_property)<-sapply(2:no_treatment, function(i)paste0("phi",i))
  method_C_property<-sapply(1:(no_treatment-1), function(i)estimator_property[[i]][[3]][,1])
  method_D_property<-sapply(1:(no_treatment-1), function(i)estimator_property[[i]][[3]][,2])
  
  method_c_property<-t(method_C_property)#[,c(1,2,5,6)]
  method_d_property<-t(method_D_property)#[,c(1,2,5,6)]
  
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
  
  estimand2_MCSE<-sqrt(estimand2[,-1]*(1-estimand2[,-1])/R )
  
  all_diff_min<-lapply(1:R, function(z){output_replication[[z]]$performance_m$diff_min })  
  
  mortality_gain<-do.call(rbind, lapply(1:R, function(z){
    apply(output_replication[[z]]$performance_m$mortality_gain, 1, function(x){sum(x*lambda)}) } )   )
  
  estimand2_MCSE<-cbind(Mortaliy=apply(mortality_gain, 2,function(x){ 
    sqrt(var(x[complete.cases(x)])/length(x[complete.cases(x)])) }), estimand2_MCSE)
  #rownames(method_c_property)<- rownames(method_d_property)<- sapply(2:10, function(i){paste0("phi_", i, "-phi_1")} )
  
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
  
  
  B2_estimates<-lapply(1:no_pattern, function(k){do.call(rbind,lapply(1:R, function(z)output_replication[[z]]$est_method_B2[,k] )) } )
  
  
  out<-list(B2_estimates=B2_estimates,
            method_C_property=method_c_property,
            method_D_property=method_d_property,
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

