library(tidyverse)
library(here)
library(lmtest) #for lrtest()

birds <- read_csv(here("data", "BankSwallows.csv"))

birds

# I misinterpreted the data last time.  `num_colonies` is the total number of colonies, `num_extinct` is the number of colonies that were abandoned the next year.  So we need a column for occupied colonies.

birds <- 
  birds %>% 
  mutate(num_occupied = num_colonies - num_extinct)
birds

#Fit 4 models to data:
#1) one extinction rate, link = "identity"
#2) one extinction rate, link = "logit"
#3) two extinction rates, by period, link = "identity"
#4) two extinction rates, by period, link = "logit"

#Compare likelihoods - should not be affected by link function here (in this case it is just a statistical tool for getting the model to fit)
#Compare coefficients for the second model - these WILL be affected by link
#Logit transform coefficients from identity model
#Inverse logit transform coefficients from link = "logit"

#Do extinction rates differ by year? Is there a yearly trend?

#one extinction rate
m0_ident <- glm(cbind(num_extinct, num_occupied) ~ 1,
               family = binomial(link = "identity"), data = birds)

m0_logit <- glm(cbind(num_extinct, num_occupied) ~ 1,
               family = binomial(link = "logit"), data = birds)

coef(m0_ident)
coef(m0_logit)

plogis(coef(m0_logit)) #this is the actual MLE value for proportion of extinct burrows

#link function doesn't affect log likelihood
logLik(m0_ident)
logLik(m0_logit)

#two extinction rates, defined by period (early vs. late)
m1_ident <- glm(cbind(num_extinct, num_occupied) ~ -1 + period,
               family = binomial(link = "identity"), data = birds)

m1_logit <- glm(cbind(num_extinct, num_occupied) ~ -1 + period,
               family = binomial(link = "logit"), data = birds)


coef(m1_ident) #using identity function, these are the exact MLE values of p by period
coef(m1_logit) #using logit function

plogis(coef(m1_logit)) #backtransform to get proportions

logLik(m1_ident)
logLik(m1_logit)
#again, doesn't affect log-likelihood

# Is there support for a change in colony extinctions after 1992?

lrtest(m0_logit, m1_logit)

# What about linear trend over time? Or year-to-year variation?  How would we write those models?

m_trend <- glm(cbind(num_extinct, num_occupied) ~ year,
               family = "binomial", data = birds)
coef(m_trend)
predict(m_trend) %>% 
  plogis()

m_year <- glm(cbind(num_extinct, num_occupied) ~ -1 + as.factor(year),
              family = binomial, data = birds)
coef(m_year) %>% plogis()

# Use AIC to find the best model
library(bbmle)
ICtab(m0_logit, m1_logit, m_trend, m_year)






