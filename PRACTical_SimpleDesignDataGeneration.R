#Open libraries
library(stringr)
library(multiwayvcov)
require(sandwich)
library(rstanarm)

# Essential functions-------------
# ------------------------------- #
# generate data for each subgroup #
# ------------------------------- #
find_phi<-function(p, alpha=-1.36){log(p/(1-p)) - alpha } 

# -------------------- #
# response probability #
# -------------------- #
res_probability<-function(phi,alpha=-1.36){
  exp(alpha+ phi)/(1+ exp(alpha+ phi) )
}

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

# ------------------------------------------------------------------------- #
# method C extension non-informative prior: fit one step model to all data  # 
# ------------------------------------------------------------------------- #
fit_onestage_C_NI<-function(alldata=Alldata){
  no_p<-no_pattern
  
  loc_NI<-0
  scale_NI<-5
  prior <- student_t(df = 7, loc_NI, scale_NI)
  prior_int <- student_t(df = 7, loc_NI, scale_NI)
  
  nma_data<-data.frame(y=unlist(alldata[1,]),
                       treatment=factor(unlist(alldata[2,]), levels = sort(unique(unlist(alldata[2,])))),
                       subgroup=factor(unlist(alldata[4,])))#patient_subgroup)))
  
  my.glm<-myTryCatch(stan_glm(y~treatment + subgroup, data = nma_data, prior = prior,
                              prior_intercept = prior_int, family = binomial(link = "logit"), 
                              cores = 2) )
  #use of default priors in stan_glm are non-informative
  
  if(is.null(my.glm$error) ) #if do not have an error, model is fitted
  { 
    my.glm<-my.glm[[1]]
    mof<-posterior_interval(my.glm, prob = 0.95)
    std.err<-sqrt(diag(vcov(my.glm))[2:no_treatment])
    out<-cbind(Estimate=my.glm$coefficients[2:no_treatment],
               model_var=std.err^2,
               z=my.glm$coefficients[2:no_treatment]/std.err,
               LL=mof[2:no_treatment, 1],
               UL=mof[2:no_treatment, 2])
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

# -------------------------------------------------------------- #
# method C extension weak prior: fit one step model to all data  # 
# -------------------------------------------------------------- #
fit_onestage_C_wk<-function(alldata_prior=Alldata_prior, alldata=Alldata){
  no_p<-no_pattern
  
  nma_data_prior<-data.frame(y=unlist(alldata_prior[1,]),
                             treatment=factor(unlist(alldata_prior[2,]), levels = sort(unique(unlist(alldata_prior[2,])))),
                             subgroup=factor(unlist(alldata_prior[4,])))#patient_subgroup)))
  my.glm_prior<-glm(y~treatment+ subgroup,family="binomial",data=nma_data_prior) 
  my.glm_prior_coeff<-my.glm_prior$coefficients
  scale_wk<-5           #Change the weight on the prior, larger scale=> less weight on prior
  prior <- normal(location = my.glm_prior_coeff[2:(no_treatment+no_p-1)], scale = rep(scale_wk, (no_treatment+no_p-2)))
  prior_int <- normal(location = my.glm_prior_coeff[1], scale = scale_wk)
  
  nma_data<-data.frame(y=unlist(alldata[1,]),
                       treatment=factor(unlist(alldata[2,]), levels = sort(unique(unlist(alldata[2,])))),
                       subgroup=factor(unlist(alldata[4,])))#patient_subgroup)))
  
  my.glm<-myTryCatch(stan_glm(y~treatment + subgroup, data = nma_data, prior = prior,
                              prior_intercept = prior_int, family = binomial(link = "logit"), 
                              cores = 2) )
  
  if(is.null(my.glm$error) ) #if do not have an error, model is fitted
  { 
    my.glm<-my.glm[[1]]
    mof<-posterior_interval(my.glm, prob = 0.95)
    std.err<-sqrt(diag(vcov(my.glm))[2:no_treatment]) 
    out<-cbind(Estimate=my.glm$coefficients[2:no_treatment],
               model_var=std.err^2,
               z=my.glm$coefficients[2:no_treatment]/std.err,
               LL=mof[2:no_treatment, 1],
               UL=mof[2:no_treatment, 2])
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

# ---------------------------------------------------------------- #
# method C extension strong prior: fit one step model to all data  # 
# ---------------------------------------------------------------- #
fit_onestage_C_str<-function(alldata_prior=Alldata_prior, alldata=Alldata){
  no_p<-no_pattern
  
  nma_data_prior<-data.frame(y=unlist(alldata_prior[1,]),
                             treatment=factor(unlist(alldata_prior[2,]), levels = sort(unique(unlist(alldata_prior[2,])))),
                             subgroup=factor(unlist(alldata_prior[4,])))#patient_subgroup)))
  my.glm_prior<-glm(y~treatment+ subgroup,family="binomial",data=nma_data_prior) 
  my.glm_prior_coeff<-my.glm_prior$coefficients
  scale_str<-1        #Change the weight on the prior, smaller scale=> more weight on prior
  prior <- normal(location = my.glm_prior_coeff[2:(no_treatment+no_p-1)], scale = rep(scale_str, (no_treatment+no_p-2)))
  prior_int <- normal(location = my.glm_prior_coeff[1], scale = scale_str)
  
  nma_data<-data.frame(y=unlist(alldata[1,]),
                       treatment=factor(unlist(alldata[2,]), levels = sort(unique(unlist(alldata[2,])))),
                       subgroup=factor(unlist(alldata[4,])))#patient_subgroup)))
  
  my.glm<-myTryCatch(stan_glm(y~treatment + subgroup, data = nma_data, prior = prior,
                              prior_intercept = prior_int, family = binomial(link = "logit"), 
                              cores = 2) )
  
  if(is.null(my.glm$error) ) #if do not have an error, model is fitted
  { 
    my.glm<-my.glm[[1]]
    mof<-posterior_interval(my.glm, prob = 0.95)
    std.err<-sqrt(diag(vcov(my.glm))[2:no_treatment])
    out<-cbind(Estimate=my.glm$coefficients[2:no_treatment],
               model_var=std.err^2,
               z=my.glm$coefficients[2:no_treatment]/std.err,
               LL=mof[2:no_treatment, 1],
               UL=mof[2:no_treatment, 2])
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
  
  #Still part of method D
  # ------------------------- **note 1** ------------------------------------------ #
  # some R version (eg R version 3.6.0 on linux) would convert the class of y to factor, 
  # if so, use the following line instead
  #fit_glm<-myTryCatch( glm(as.numeric(levels(y))[y]~treatment+id_comparison,family="binomial",data=dup_data) )
  
  fit_glmm<-myTryCatch( glm(as.numeric(y)~treatment+id_comparison,family="binomial",data=dup_data) )
  
  if(is.null(fit_glm$error) ) #if there is no error, model is fitted 
  { 
    fit_glmm<-fit_glmm[[1]]
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

# -------------------- #
# Current design data generation -------------
# -------------------- #
# 4 treatment 4 pattern data simulation
no_treatment=4 
# 1. ceftazidime-avibactam
# 2. ceftazidime-avibactam + aztreonam
# 3. cefiderocol
# 4. colistin + meropenem

# treatment patterns
pattern1<- 1:3 # Patient with Kidney impairment + KPC culture
pattern2<- c(2, 3)# Patient with Kidney impairment + NDM culture
pattern3<- 1:4 # Patient without Kidney impairment + KPC culture
pattern4<- 3:4 # Patient without Kidney impairment + NDM culture
patternV<-list(pattern1, pattern2, pattern3, pattern4)

# treatment effect parameters in prior trial------------
alpha_prior <- find_phi(0.35, alpha=0) #baseline risk = 35%
phi_prior <- find_phi(seq(0.2, 0.5, 
                    length.out = no_treatment), 
                    alpha = alpha_prior) #treatment risk = 20-50%
res_rate_prior <- res_probability(phi_prior,alpha_prior)

res_rate_mat_prior <- matrix(res_rate_prior, byrow = T,
                     nrow = length(patternV), 
                     ncol = no_treatment)

# response rate: row= pattern, column=treatment. All rows have same values for this scenario
res_probability_prior=res_rate_mat # response probability
colnames(res_probability_prior) <- sapply(1:no_treatment, function(i){paste0("treatment_", i)} )
rownames(res_probability_prior) <- sapply(1:length(pattern), function(i){paste0("alpha_", i)} )

# treatment effect parameters in current trial------------
alpha_1 <- find_phi(0.35, alpha=0) #baseline risk = 35%
phi_1 <- find_phi(seq(0.2, 0.5, 
                    length.out = no_treatment), 
                    alpha = alpha_1) #treatment risk = 20-50%
res_rate1 <- res_probability(phi_1,alpha_1)

res_rate_mat <- matrix(res_rate1, byrow = T,
                     nrow = length(patternV), 
                     ncol = no_treatment)
prob_pattern= c(0.25, 0.25, 0.25, 0.25) # prevalence rate of patterns (currently assume equal, 0.25 in each)

N=1000 # total number of patients
R=5 # number of trial replications

phi_v=phi_1 # true parameters of treatment effect
pattern=patternV # personalized randomization lists

# response rate: row= pattern, column=treatment. All rows have same values for this scenario
res_probability_all=res_rate_mat # response probability
colnames(res_probability_all) <- sapply(1:no_treatment, function(i){paste0("treatment_", i)} )
rownames(res_probability_all) <- sapply(1:length(pattern), function(i){paste0("alpha_", i)} )

# Prepare for data generation
no_pattern <- length(pattern) # number of randomization lists 

# for each randomization list, the number of pairwise comparisons fixing a reference treatment. 
no_comparison <- sapply(1:no_pattern, function(i){length(pattern[[i]])-1})

no_treatment <-length(unique(unlist(pattern))) # number of treatments

# each person has prob_pattern to be allocated to one of the treatment patterns
assigned_pattern<-t(rmultinom(N, size=1, prob_pattern))
colnames(assigned_pattern)<-sapply(1:no_pattern, function(i){paste0("subgroup", i)} )
# number of patients in each subgroup that is defined by the pattern
size_pattern <-apply(assigned_pattern, 2, sum)

# generate one historical dataset----
Alldata_prior<-
  sapply(1:no_pattern, function(i){
  generate_subset_data(i, size_pattern.=size_pattern, 
                       pattern.=pattern, 
                       res_probability_all.=res_probability_prior)})

# generate one current dataset----
Alldata<-
  sapply(1:no_pattern, function(i){
  generate_subset_data(i, size_pattern.=size_pattern, 
                       pattern.=pattern, 
                       res_probability_all.=res_probability_all)})

# show how many have been randomized to a treatment arm within a pattern
feq_t_subgroup<-sapply(1:no_pattern, function(i)table(Alldata[2,][[i]]))

# show how many have been randomized to each treatment arm
feq_t<-table(unlist(Alldata[2,]))

est_method_C<-fit_onestage_C(Alldata) # use original current data
est_method_C_NI<-fit_onestage_C_NI(Alldata) # use original current data
est_method_C_wk<-fit_onestage_C_wk(Alldata_prior, Alldata) # use original current data + original prior data
est_method_C_str<-fit_onestage_C_str(Alldata_prior, Alldata) # use original current data + original prior data
est_method_D<-fit_robustSE_D(Alldata) # use duplicated current data
      
# combine estimated best treatments from all methods, row= methods, column= pattern
identified_best_t<-rbind(method_C=est_method_C$ranking[1,],
                         method_C_NI=est_method_C_NI$ranking[1,],
                         method_C_wk=est_method_C_wk$ranking[1,],
                         method_C_str=est_method_C_str$ranking[1,],
                         method_D=est_method_D$ranking[1,] )
                       
                       
