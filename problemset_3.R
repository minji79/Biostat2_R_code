#problem set 3

###### part. 1 #######
library(tidyverse)
nepal621 = read_csv("nepal621_v2.csv")

#method 1. create table
nepal621 %>%
  group_by(trt, sex, age) %>%
  summarize(N_Alive = sum(status=="Alive"),
            Perc_Alive = N_Alive/n(),
            N_Died = sum(status=="Died"),
            Perc_Died = N_Died/n(),
            Total=n())

## Make it a little prettier!
nepal621 %>%
  group_by(trt, sex, age) %>%
  summarize(N_Alive = sum(status=="Alive"),
            Perc_Alive = round(N_Alive/n(),4)*100,
            N_Died = sum(status=="Died"),
            Perc_Died = round(N_Died/n(),4)*100,
            Total=n())
#method 2
nepal621 %>%
  group_by(trt) %>%
  summarize(N_Alive = sum(status=="Alive"),
            Perc_Alive = round(N_Alive/n(),4)*100,
            N_Died = sum(status=="Died"),
            Perc_Died = round(N_Died/n(),4)*100,
            Total=n())

#method 3 - 95%ci
nepal621 %>%
  group_by(trt) %>%
  summarize(N_Alive = sum(status=="Alive"),
            p_Alive = N_Alive/n(),
            N_Died = sum(status=="Died"),
            p_Died = N_Died/n(),
            Total=n(),
            se_Died = sqrt(p_Died *(1-p_Died)/Total),
            CI_L = p_Died - 1.96*se_Died,
            CI_U = p_Died + 1.96*se_Died)

p.1 = 290/13389 # fill in sample proportion for first sample
n.1 = 13389 # fill in sample size for first sample
p.2 = 233/13732 # fill in sample proportion for second sample
n.2 = 13732 # fill in sample size for second sample

diff = p.1 - p.2
se = sqrt(p.1*(1-p.1)/n.1 + p.2*(1-p.2)/n.2)       # standard error
diff - 1.96*se; diff + 1.96*se   # confidence interval

#method 4 - 95% ci for the difference in mortality rates for the vitamin A and control groups separately for each age-sex stratum.
nepal621 %>%
  group_by(sex, age, trt) %>%
  summarize(N_Died = sum(status=="Died"),
            p_Died = N_Died/n(),
            Total = n())
## alternatively, calculate the CIs directly within each age/sex strata
nepal621 %>%
  group_by(sex, age) %>%
  summarize(N_Plac = sum(trt=="Placebo"),
            p_Plac = sum(status=="Died" & trt=="Placebo")/N_Plac,
            N_VitA = sum(trt=="Vit A"),
            p_VitA = sum(status=="Died" & trt=="Vit A")/N_VitA,
            diff = p_Plac - p_VitA,
            se = sqrt(p_Plac*(1 - p_Plac)/N_Plac + p_VitA*(1 - p_VitA)/N_VitA),
            CI_L = diff - 1.96*se,
            CI_U = diff + 1.96*se)
            
#draw the graph for the above 95% ci
install.packages("Hmisc")
library(Hmisc)

dataForCIplot = nepal621 %>%
  group_by(sex, age) %>%
  dplyr::summarize(N_Plac = sum(trt=="Placebo"),
            p_Plac = sum(status=="Died" & trt=="Placebo")/N_Plac,
            N_VitA = sum(trt=="Vit A"),
            p_VitA = sum(status=="Died" & trt=="Vit A")/N_VitA,
            diff = p_Plac - p_VitA,
            se = sqrt(p_Plac*(1 - p_Plac)/N_Plac + p_VitA*(1 - p_VitA)/N_VitA),
            CI_L = diff - 1.96*se,
            CI_U = diff + 1.96*se)

agestrata = c(1,2,3,4,5,6,7)
agestrata_labels = c("F < 1", "F 1-2", "F 3-4", "M < 1", "M 1-2", "M 3-4", "Overall")
diff = c(dataForCIplot$diff, 0.0047)
LL = c(dataForCIplot$CI_L, 0.00142)
UL = c(dataForCIplot$CI_U, 0.00798)

## Add labels to the axes
errbar(x = agestrata,
       y = diff,
       yplus = LL,
       yminus = UL,
       xaxt = "n",     #xaxt removes the numberic lables
       xlab = "Age/Gender Group",  #label for x-axis
       ylab = "Difference in Mortality Rates (Placebo - VitA)")  #label for y-axis()

## Add a title
title(main="95% Confidence Intervals for Difference in Mortality Rates")

## Add group labels for the age-gender groups
axis(side=1, #1 = the bottom of graph
     at=agestrata,   #where on x-axis; same as "x" in errbar
     labels=agestrata_labels)  #what the labels are

# Add horizontal line at zero
abline(h=0, col="red")

#method 5 : linear regression
model1 = glm(as.factor(status) ~ trt, data=nepal621, family=binomial(link="identity"))
summary(model1)
confint(model1)

###### part. 2 #######
library(tidyverse)
nepal621 = read_csv("nepal621.csv")

#method 1
nepal621 = nepal621 %>%
  mutate(agegp = ifelse(age == "3-4", "3+ years", "<3 years"))

nepal621

# Calculates the odds by age group and trt; can the find CI by hand
nepal621 %>%
  group_by(agegp, trt) %>%
  dplyr::summarize(N_Alive = sum(status=="Alive"),
            N_Died = sum(status=="Died"),
            Odds = N_Died/N_Alive)

# Let R do all the calculations for you!
nepal621 %>%
  group_by(agegp) %>%
  dplyr::summarize(N_Alive_P = sum(status=="Alive" & trt=="Placebo"),
            N_Died_P = sum(status=="Died" & trt=="Placebo"),
            N_Alive_V = sum(status=="Alive" & trt=="Vit A"),
            N_Died_V = sum(status=="Died" & trt=="Vit A"),
            OR = (N_Died_V/N_Alive_V)/(N_Died_P/N_Alive_P),
            se = sqrt(1/N_Alive_P + 1/N_Died_P + 1/N_Alive_V + 1/N_Died_V),
            CI_L = exp(log(OR)-1.96*se),
            CI_U = exp(log(OR)+1.96*se))

  
#method 3. seperately for each age startum, estimate the odds ratio by using a logistic regression of the binary survival indicator
nepal621.lowage = nepal621 %>% filter(agegp == "<3 years")
model2 = glm(as.factor(status) ~ trt, data=nepal621.lowage,
             family=binomial(link="logit"))

summary(model2)  # This summary is on the logOR scale
exp(model2$coefficients)  # We exponentiate to get on the OR scale
exp(confint(model2))  # We can also exponentiate the CI to the OR scale

nepal621.highage = nepal621 %>% filter(agegp == "3+ years")
model3 = glm(as.factor(status) ~ trt, data=nepal621.highage,
             family=binomial(link="logit"))

summary(model3)
exp(model3$coefficients)
exp(confint(model3))
             

             

            