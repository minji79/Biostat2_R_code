#problem set4

library(tidyverse)
nepalibf = read_csv("nepalibf.csv")
head(nepalibf)
summary(nepalibf)
table(nepalibf$bf)

#1.
boxplot(age_chld ~ bf, data=nepalibf, ylab="Age of child (months)")
boxplot(age_mom ~ bf, data=nepalibf)
boxplot(parity ~ bf, data=nepalibf)

#2
CT = xtabs(~ sex_chld + bf, data=nepalibf)
addmargins(CT)
prop.table(CT, margin=1)
addmargins(prop.table(CT, margin=1), margin=2)

#3
#4
nepalibf = nepalibf %>%
  mutate(agechldc = age_chld - mean(age_chld))
model1 = glm(bf ~ sex_chld + agechldc, data=nepalibf,family=binomial(link="logit"))
summary(model1)
exp(model1$coefficients)
exp(confint.default(model1))

#5
modelE = glm(bf~agechldc+sex_chld+sex_chld:agechldc, data=nepalibf, family=binomial(link="logit"))
summary(modelE)

#6
nepalibf = nepalibf %>%
  na.omit() %>%  # Remove observations with missing data
  mutate(sex_chld=recode_factor(sex_chld, `0`="Male", `1`="Female")) # Factor sex_chld

modelD = glm(bf ~ sex_chld + age_chld, data=nepalibf, family=binomial(link="logit"))
nepalibf = nepalibf %>% mutate(phat = predict(modelD, type="response"))

qplot(x=age_chld, y=phat, color=sex_chld, shape=sex_chld, data=nepalibf,
      xlab="Child's age in months", ylab="Predicted prevalence of Breast-feeding")

#7
#install.packages("ResourceSelection") # only if haven't installed yet library(ResourceSelection)
install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(nepalibf$bf, nepalibf$phat, g=10)
hoslem.test(nepalibf$bf, nepalibf$phat)$observed
hoslem.test(nepalibf$bf, nepalibf$phat)$expected


