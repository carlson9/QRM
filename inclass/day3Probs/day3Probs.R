# Probability distributions in \texttt{R}

#always set a seed when simulating data
set.seed(1454)
#draw from a standard normal
rnorm(1)
#draw 10 from a standard normal
rnorm(10)
#plot a density
plot(density(rnorm(10)))
plot(density(rnorm(10000)))
#change the mean and standard deviation
plot(density(rnorm(10000, mean = 5, sd = 3)))
#normal cumulative density function
pnorm(-Inf)
pnorm(Inf)
pnorm(0)
plot(pnorm(seq(-3, 3, by = .01)))
plot(pnorm(seq(-3, 3, by = .01)), type = 'l')
plot(seq(-3, 3, by = .01), pnorm(seq(-3, 3, by = .01)), type = 'l')
#normal probability density function
dnorm(-Inf)
dnorm(Inf)
dnorm(0)
plot(dnorm(seq(-3, 3, by = .01)))
plot(seq(-3, 3, by = .01), dnorm(seq(-3, 3, by = .01)), type = 'l')
#normal inverse CDF
qnorm(.5)
qnorm(.025)
qnorm(1 - .025)
qnorm(.025, lower = TRUE)
qnorm(.025, lower = FALSE)
plot(seq(0, 1, by = .01), qnorm(seq(0, 1, by = .01)), type = 'l')
#gamma pdf
rgamma(1, shape = 1)
plot(density(rgamma(10000, shape = 1)))
plot(density(rgamma(10000, shape = 10)))
plot(seq(0, 10, by = .01), dgamma(seq(0, 10, by = .01), shape = 1), type = 'l')
#gamma CDF
plot(seq(0, 10, by = .01), pgamma(seq(0, 10, by = .01), shape = 1), type = 'l')
#binomial
rbinom(1, 1, .5)
rbinom(1, 10, .5)
rbinom(n = 100, size = 10, prob = .5)
#sampling
sample(c(0,1), 1, prob = c(.8, .2))
sample(c(0,1), 100, prob = c(.8, .2), replace = TRUE)
#uniform
runif(100)
#Weibull
rweibull(1, shape = 1, scale = 1)
rweibull(1, shape = 10, scale = 10)
#sample from uniform to generate any distribution using the inverse CDF
u = runif(10000)
samps = qweibull(u, 10, 10)
plot(density(samps))
plot(density(rweibull(10000, 10, 10)))
plot(density(qnorm(u)))
#generate random variance-covariance matrices
rWishart(1, 10, toeplitz((10:1)/10))
sig = rWishart(1, 10, toeplitz((10:1)/10))
#mutlivariate normal
library(mgcv)
samps = rmvn(1000, mu = rep(0, 10), V = sig[,,1])
cov(samps)
sig
#generate auto-regressive variables
rho = .8
n = 5
sig = diag(1, n)
for(i in 1:n){
  for(k in 1:n){
    sig[i, k] = sig[k, i] = rho^(i-k)
  }
}
sig
samps = rmvn(1000, mu = rep(0, n), V = sig)
cov(samps)
#convert to a correlation matrix
library(MBESS)
cov2cor(cov(samps))
#generate more explicit correlations
#if x1 and x2 are correlated rho, x1 ~ rho*x2 + sqrt(1-rho^2)*F,
#   where F is the distribution
rho = .6
x1 = rnorm(1000)
x2 = rho * x1 + sqrt(1 - rho^2)*rnorm(1000)
cor(x1, x2)
#autocorrelation
err = rnorm(1)
n = 20
for(i in 2:n) err[i] = rho * err[i - 1] + sqrt(1 - rho^2)*rnorm(1)
plot(1:20, err, type = 'l')
mean(err)
sd(err)
err.scaled = scale(err)
plot(1:20, err.scaled, type = 'l')
mean(err.scaled)
sd(err.scaled)



# Central Limit Theorem

# - (From Wiki) In probability theory, the central limit theorem (CLT) establishes that, in some situations, when independent random variables are added, their properly normalized sum tends toward a normal distribution (informally a bell curve) even if the original variables themselves are not normally distributed. The theorem is a key concept in probability theory because it implies that probabilistic and statistical methods that work for normal distributions can be applicable to many problems involving other types of distributions.
# 
# - If $X_{1},X_{2},\ldots,X_{n}$ are random samples each of size $n$ taken from a population with overall mean $\mu$  and finite variance $\sigma ^{2}$ and if $\bar {X}$ is the sample mean, the limiting form of the distribution of $Z=\left({\frac {{\bar {X}}_{n}-\mu }{\sigma /\surd n}}\right)$  as $n\to \infty$, is the standard normal distribution.

#remember the gamma
plot(seq(0, 10, by = .01), dgamma(seq(0, 10, by = .01), shape = 10), type = 'l')
samps = lapply(1:1000, function(x) rgamma(1000, shape = 10))
class(samps)
means = unlist(lapply(samps, mean))
plot(density(means))
samps.bar = mean(unlist(samps))
sig = var(unlist(samps))
means.normalized = (means - samps.bar)/(sqrt(sig / 1000))
plot(density(means.normalized))
#what about binomial?
plot(density(rbinom(1000, 1, .8)))
samps = lapply(1:1000, function(x) rbinom(1000, size = 1, prob = .8))
class(samps)
means = unlist(lapply(samps, mean))
plot(density(means))
samps.bar = mean(unlist(samps))
sig = var(unlist(samps))
means.normalized = (means - samps.bar)/(sqrt(sig / 1000))
plot(density(means.normalized))
#are they from a standard normal?
t.test(means.normalized, rnorm(1000))
shapiro.test(means.normalized)
#non-parametric
ks.test(means.normalized, rnorm(1000))

# Simulation exercises

# - Write a function that takes as an argument $n$ and returns an $n\times 10$ matrix $X$
# - The columns of $X$ should be independently but not identically distributed (get creative)
# - Write a second function that takes $X$ and generates an $n\times 1$ outcome $y$ through a linear combination of the variables (only include three in the calculation) plus i.i.d. normally distributed errors
# - Write a third function that takes as an argument $n.samps$, generates $n.samps$ $X$ matrices and $y$ outcomes using the above functions, runs a linear regression, and returns coverage probabilities, false positive rates, and false negative rates
# - Next, alter the functions to violate assumptions: Create a correlated but unrelated $X$ column, create autocorrelated errors, etc.
# - Assess the returns of the functions at various levels of $n.samps$ and $n$
# - Plot the results

set.seed(999)
genX = function(n = 100){
  X1 = rnorm(n)
  X2 = rbinom(n, 1, .7)
  X3 = as.vector(rWishart(n, 1, as.matrix(1)))
  X4 = rgamma(n, 5, 5)
  X5 = rnorm(n, 4, 2)
  X6 = sample(c(0,1,2,3,4), n, prob = c(.5, .3, .1, .075, .025), replace = TRUE)
  X7 = rweibull(n, 10)
  X8 = runif(n, 0, 10)
  X9 = rnorm(n, 40, 5)
  X10 = rchisq(n, 10)
  X = cbind(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)
  return(X)
}
genY = function(X){
  return(X[, 1:3] %*% c(.1, 4, -.7) + rnorm(ncol(X)))
}
tester = function(n.samps = 1000){
  beta = c(.1, 4, -.7, rep(0, 7))
  covs = matrix(nrow = n.samps, ncol = 10)
  falsePos = matrix(nrow = n.samps, ncol = 7)
  falseNeg = matrix(nrow = n.samps, ncol = 3)
  for(i in 1:n.samps){
    X = genX()
    y = genY(X)
    mod = lm(y ~ X)
    intMat = confint(mod)[-1,]
    covs[i,] = unlist(lapply(1:10, function(x) intMat[x, 1] < beta[x] & intMat[x, 2] > beta[x]))
    falsePos[i,] =  unlist(lapply(1:7, function(x) intMat[x + 3, 1] > 0 | intMat[x + 3, 2] < 0))
    falseNeg[i,] =  unlist(lapply(1:3, function(x) intMat[x, 1] < 0 & intMat[x, 2] > 0))
  }
  covRate = mean(covs)
  fpRate = mean(falsePos)
  fnRate = mean(falseNeg)
  return(c(covRate, fpRate, fnRate))
}
#now alteration
genXalt = function(n = 100){
  X1 = rnorm(n)
  X2 = rbinom(n, 1, .7)
  X3 = as.vector(rWishart(n, 1, as.matrix(1)))
  X4 = .8 * X1 + sqrt(1 - .8^2)*rgamma(n, 5, 5)
  X5 = .8 * X1 + sqrt(1 - .8^2)*rnorm(n, 4, 2)
  X6 = sample(c(0,1,2,3,4), n, prob = c(.5, .3, .1, .075, .025), replace = TRUE)
  X7 = .8 * X2 + sqrt(1 - .8^2)*rweibull(n, 10)
  X8 = runif(n, 0, 10)
  X9 = .8 * X3 + sqrt(1 - .8^2)*rnorm(n, 40, 5)
  X10 = .8 * X2 + sqrt(1 - .8^2)*rchisq(n, 10)
  X = cbind(X1, X2, X3, X4, X5, X6, X7, X8, X9, X10)
  return(X)
}
genYalt = function(X){
  rho = .8
  err = rnorm(1)
  for(i in 2:nrow(X)) err[i] = rho * err[i - 1] + sqrt(1 - rho^2)*rnorm(1) #this won't cause too many errors because there's no other related component (e.g., not TSCS)
  err.scaled = scale(err)
  return(X[, 1:3] %*% c(.1, 4, -.7) + err.scaled)
}
testerAlt = function(n.samps = 1000){
  beta = c(.1, 4, -.7, rep(0, 7))
  covs = matrix(nrow = n.samps, ncol = 10)
  falsePos = matrix(nrow = n.samps, ncol = 7)
  falseNeg = matrix(nrow = n.samps, ncol = 3)
  for(i in 1:n.samps){
    X = genXalt()
    y = genYalt(X)
    mod = lm(y ~ X)
    intMat = confint(mod)[-1,]
    covs[i,] = unlist(lapply(1:10, function(x) intMat[x, 1] < beta[x] & intMat[x, 2] > beta[x]))
    falsePos[i,] =  unlist(lapply(1:7, function(x) intMat[x + 3, 1] > 0 | intMat[x + 3, 2] < 0))
    falseNeg[i,] =  unlist(lapply(1:3, function(x) intMat[x, 1] < 0 & intMat[x, 2] > 0))
  }
  covRate = mean(covs)
  fpRate = mean(falsePos)
  fnRate = mean(falseNeg)
  return(c(covRate, fpRate, fnRate))
}



