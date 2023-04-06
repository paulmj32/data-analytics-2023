library(rstanarm)
library(rstan)
library(tidyverse)

############################################################################################
### LINEAR MODEL 
############################################################################################
# Data
data(cars)
head(cars)

plot(cars$speed, cars$dist)
cor(cars$speed, cars$dist)

# Linear regression
linear = lm(dist ~ speed, data = cars)
summary(linear)
lines(cars$speed, linear$fitted.values, col = "red")

# 95% confidence interval 
confint(linear, level = 0.95)


#rsq = cor(glm_lin$fitted.values, cars$dist)^2
#rsq = 1 - sum((cars$dist - glm_lin$fitted.values)^2) / sum((cars$dist - mean(cars$dist))^2)
#r = rsq^(.5)

# conf_interval = predict(linear, newdata=data.frame(speed = cars$speed), interval="confidence", level = .95)
# 
# df.gg1 = data.frame(speed = cars$speed,
#                     dist = cars$dist,
#                     yhat = linear$fitted.values,
#                     CI.lower = conf_interval[,2],
#                     CI.upper = conf_interval[,3])
# ggplot(data = df.gg1) +
#   geom_point(aes(x = speed, y = dist)) +
#   geom_line(aes(x = speed, y = yhat), col = "red") +
#   geom_ribbon(aes(x = speed, ymin = CI.lower, ymax = CI.upper), fill = "red", alpha = 0.5)


############################################################################################
### BAYESIAN MODEL 
############################################################################################
# Bayesian linear model
bayes = stan_glm(dist ~ speed, data = cars, family = "gaussian")
summary(bayes)

# Diagnostics
stan_trace(bayes, pars=c("(Intercept)","speed","sigma"))
mean(cars$dist) #matches mean_PPD

# Posterior
posterior = as.data.frame(bayes)
stan_hist(bayes, pars=c("(Intercept)","speed","sigma"))
summary(posterior)

mean_speed = mean(posterior$speed) 


############################################################################################
### CREDIBLE INTERVALS 
############################################################################################
# Confidence Intervals (95%) 
CI_95.lin = confint(linear)
CI_95.lin

# Credible Intervals (95%)
CI_95.bayes = posterior_interval(bayes, prob = 0.95)
CI_95.bayes

ci_speed = quantile(posterior$speed, probs=c(0.025, 0.975)) # posterior 95% interval 


## Q1: How do you interpret a confidence interval vs credible interval?
## A1: see slides 

#Bonus Question: How would you calculate prediction intervals for bayesian?  
PI_95.bayes = predictive_interval(bayes, prob = 0.95)
PI_95.bayes

############################################################################################
### LET's TALK ABOUT PRIORS 
############################################################################################
# Default priors ... do these make sense? (Default are week priors)
prior_summary(bayes)

# normal: 2.5 --> standard deviations 
# exponential: mean ~ 1/lambda ... 1/1 --> mean standard deviation
1/0.039
sd(cars$dist)

# Strong Priors
bayes.strong = stan_glm(dist ~ speed, data = cars, family = "gaussian",
                        prior_intercept = normal(-15, 7, autoscale = F),
                        prior = normal(2, 0.5, autoscale = F),
                        prior_aux = exponential(1/.05, autoscale = F)
                        )

posterior.strong = as.data.frame(bayes.strong)
stan_hist(bayes.strong, pars=c("(Intercept)","speed","sigma"))
summary(posterior.strong)



