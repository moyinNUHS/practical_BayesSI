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

# treatment effect parameters------------
alpha_1 < -find_phi(0.35, alpha=0) #baseline risk = 35%
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


# generate one dataset----
Alldata<-
  sapply(1:no_pattern, function(i){
  generate_subset_data(i, size_pattern.=size_pattern, 
                       pattern.=pattern, 
                       res_probability_all.=res_probability_all)})

# show how many have been randomized to a treatment arm within a pattern
feq_t_subgroup<-sapply(1:no_pattern, function(i)table(Alldata[2,][[i]]))

# show how many have been randomized to each treatment arm
feq_t<-table(unlist(Alldata[2,]))

