# reference: http://www.ievbras.ru/ecostat/Kiril/R/Biblio_N/R_Eng/Bretz2011.pdf
library(multcomp)

# simulate data 
d = data.frame(id = 1:400,
               treatment = as.factor(rep(c('A', 'B', 'C', 'D'), each = 100)),
               outcome = as.numeric(c(rep(c('0', '1'), c(50, 50)), # TA = 0.5
                                     rep(c('0', '1'), c(10, 90)), # TA = 0.1
                                     rep(c('0', '1'), c(30, 70)), # TA = 0.3
                                     rep(c('0', '1'), c(40, 60)))) # TA = 0.4
)

head(d)

# model with GLM
model = glm(outcome ~ treatment, data = d)
coef(model)

# retrieve absolute effects when reference levels are changed
# d$treatment = relevel(d$treatment, ref = 'B')
# str(d)
# model = glm(outcome ~ treatment, data = d, family = "binomial")
# summary(model)
# 
# abs_eff = list()
# for (t in c('A', 'B', 'C', 'D')) {
# # The predicted value for this condition: the intercept, plus the dummy coefficient for this condition
# Yhat <- coef(model)["(Intercept)"] + coef(model)[paste0("treatment", t)]
# if (is.na(Yhat)){Yhat <- coef(model)["(Intercept)"]}
# # Converting the predicted value from log odds into percentage
# abs_eff[[t]] = 1 - exp( Yhat ) / ( 1 + exp( Yhat ) )
# }
# abs_eff

# Dunnett test 
dunnett_test = glht(model, 
                    linfct = mcp(treatment = "Dunnett"),
                    alternative = c("two.sided"))
summary(dunnett_test)
# Multiplicity adjusted p-values are reported in the last column. 
# By default, these p-values are calculated from
# the underlying multivariate t distribution (thus accounting for the correlations
# between the test statistics) and can be compared directly with the pre-specified
# significance level α = 0.05

# Step-down Dunnett test 
stepdown = summary(dunnett_test, test = adjusted(type = "free"))

# Output of p values 
data.frame(unadj = round(summary(model)$coeff[2:4,4], 5), 
           dunnett = round(summary(dunnett_test)$test$pvalues, 5), 
           stepdowndunnett = round(summary(stepdown)$test$pvalues, 5))
# Dunnett and stepdown Dunnett inflate p value hence reducing type 1 error 
