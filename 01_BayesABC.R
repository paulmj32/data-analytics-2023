###############################################################################
### MY FIRST (AND LAST) BAYESIAN MODEL 
###############################################################################
# Data 
friends = 7
n = 10

# Prior
set.seed(32)
prior = runif(1000, 0, 1)
hist(prior)

# Likelihood (Generating Function)
likelihood = rbinom(1000, n, prior)

# Get Posterior (i.e., update prior to reflect data)
posterior = prior[which(likelihood == friends)]
hist(posterior)

## Q1: What is this starting to look like (hint: look at the likelihood function)?
## A1: Binomial distribution (n = 10, p = 0.7)

# Let's try to get a cleaner approximation of the posterior and find out? 
samples = 10000 
set.seed(32)
prior = runif(samples, 0, 1)
likelihood = rbinom(samples, n, prior)
posterior = prior[which(likelihood == friends)]
hist(posterior)

## Q2: If we do this w/ infinitely many samples, what do you think the highest point (mode) would be?
## Q2 contd.: The mode of a posterior distribution with uniform prior will produce ... ?
## A2: The Maximum Likelihood Estimate (if prior is uniform ... i.e., null prior)

###############################################################################
### TRYING DIFFERENT PRIORS (bell-shape)
###############################################################################
# # Define prior - normal 
# set.seed(32)
# prior.norm = rnorm(samples, mean = 0, sd = 1)
# hist(prior.norm)
# 
# # Likelihood
# likelihood.norm = rbinom(samples, n, prior.norm)
# 
# #scale our prior (0,1)
# fun.minmax = function(x){
#   (x - min(x)) / (max(x) - min(x))
# }
# prior.norm.scale = fun.minmax(prior.norm)
# likelihood.norm = rbinom(samples, n, prior.norm.scale)
# 
# # Get Posterior
# posterior.norm = prior.norm.scale[which(likelihood.norm == friends)]
# hist(posterior.norm)

###############################################################################
### TRYING DIFFERENT PRIORS (informative prior)
###############################################################################
# informative prior: you're not as cool as you think you are ... don't have many friends 
set.seed(32)
prior.beta = rbeta(samples, 2, 5)
hist(prior.beta)

#likelihood
likelihood.beta = rbinom(samples, n, prior.beta)

# get posterior
posterior.beta = prior.beta[which(likelihood.beta == friends)]
hist(posterior.beta)

## Q3: What's different about the two posteriors?
## A3: The second is shifted toward the informative prior
hist(posterior)
hist(posterior.beta)
summary(posterior)
summary(posterior.beta)

###############################################################################
### MORE DATA --> Through another party 
###############################################################################
friends2 = 8 
prior2 = sample(posterior.beta, size = samples, replace = T)
likelihood2 = rbinom(samples, n, prior2)
posterior2 = prior2[which(likelihood2 == friends2)]
hist(posterior2)

##Q4: What happens to our posterior as we get more data? 
##A4: We get closer to the Likelihood function


###############################################################################
### OH YEAH, WHAT ABOUT THAT NORMALZING?
###############################################################################
#https://mq-software-carpentry.github.io/statistics-with-r/06-bayesian-statistics/index.html
barplot(table(cut(posterior2, seq(0, 1, 0.05))) / 1, col = "skyblue")
barplot(table(cut(posterior2, seq(0, 1, 0.05))) / length(posterior2), col = "salmon")


###############################################################################
### SOME DOWNSIDES TO THIS APPROACH 
###############################################################################
friends3 = 70
n3 = 100

set.seed(32)
prior.beta3 = rbeta(samples, 2, 5)
likelihood.beta3 = rbinom(samples, n3, prior.beta3)
posterior.beta3 = prior.beta3[which(likelihood.beta3 == friends3)]
hist(posterior.beta3)

## Uh oh ... 
## Q5: What is going on? What are we effectively doing?
## A5: It's much harder to get exactly 70 out of 100 than 7 out of 10 
## A5contd: We're starting to turn it into a continuous variable (which has P[X=x] = 0)

friends4 = 70
n4 = 100
samples4 = 1000000
set.seed(32)
prior.beta4 = rbeta(samples4, 2, 5)
likelihood.beta4 = rbinom(samples4, n4, prior.beta4)
posterior.beta4 = prior.beta4[which(likelihood.beta4 == friends4)]
hist(posterior.beta4)

# Notice, we have more specific data, so we shift more toward the likelihood
hist(posterior.beta)
hist(posterior.beta4)

