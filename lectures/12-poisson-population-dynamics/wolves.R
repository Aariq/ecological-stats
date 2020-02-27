# load packages
library(tidyverse)
library(here)

# Load data
wolves <- read_csv(here("data", "NRMwolves.csv"))
head(wolves)

# create year_post
wolves <- wolves %>%
  mutate(year_post = year - 1982)

# Plot total wolves vs. time
wolfplot <-
  ggplot(wolves, aes(x = year, y = log(num.wolves))) +
  geom_point() 
wolfplot

# Try fitting a straight line with link = "identity"
m1 <- glm(num.wolves ~ year, family = poisson(link = "identity"), data = wolves)
# results in an error

# Exponential growth model using log-link (observation error)
m_exp <- glm(num.wolves ~ year_post, 
             family = poisson(link = "log"), data = wolves)
coef(m_exp) #on link-scale
exp(coef(m_exp)) #N_0 and lambda
## Plot predicted values
wolfplot +
  geom_line(aes(y = log(predict(m_exp, type = "response"))), color = "darkgreen")

# Process error model using offset
## First need to create lagged population size variable
wolves2 <- wolves %>%
  mutate(num.prev = lag(num.wolves)) %>% 
  filter(!is.na(num.prev)) #removes 1982. You could also use filter(year != 1982)
head(wolves2)

## fit model
m_process <- glm(num.wolves ~ 1, offset = log(num.prev),
                 family = poisson(link = "log"), data = wolves2)

exp(coef(m_process)) #growth rate N_t+1 = N_t*1.108

## Plot predicted values
wolfplot2 <- ggplot(wolves2, aes(x = year, y = num.wolves)) +
  geom_point()
wolfplot2 +
  geom_line(aes(y = predict(m_process, type = "response")), color = "orange")

# Ricker model (with process error)
m_rick <- glm(num.wolves ~ num.prev, offset = log(num.prev),
              family = poisson, data = wolves2)
coef(m_rick)

## Plot predicted values
wolfplot2 +
  geom_line(aes(y = predict(m_rick, type = "response")), color = "brown")

# Quadratic model
m_quad <- glm(num.wolves ~ year_post + I(year_post^2), 
              family = poisson, data = wolves2)
coef(m_quad)
## Plot predicted values
wolfplot2 +
  geom_line(aes(y = predict(m_quad, type = "response")), color = "purple")


# Model competition
nrow(wolves) == nrow(wolves2)
## Need to re-fit exponential model with wolves2
m_exp2 <- glm(num.wolves ~ year_post, family = poisson, data = wolves2)

wolfplot2 +
  geom_line(aes(y = predict(m_exp2, type = "response")), color = "darkgreen") +
  geom_line(aes(y = predict(m_process, type = "response")), color = "orange") +
  geom_line(aes(y = predict(m_rick, type = "response")), color = "brown") +
  geom_line(aes(y = predict(m_quad, type = "response")), color = "purple") 



## Use AIC since models are not nested
AIC(m_exp2, m_process, m_rick, m_quad)
library(bbmle)
ICtab(m_exp2, m_process, m_rick, m_quad)
## Ricker model wins with lowest AIC
## Both density dependent growth models are better than both models with fixed growth rates.

