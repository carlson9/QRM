rm(list=ls())
setwd('~/QRM/inclass/day8fitStats/')
data = read.csv('../day6GLMs/data.csv')
pop_logit = glm(intercon ~ country_pop + aggdifxx + gdppc + polity2, 
                 data=data,
                 family = binomial)


# Bootstrapping
iters = 1000
coefs = matrix(nrow = iters, ncol = 5)
dataComplete = na.omit(data[, c('intercon', 'country_pop', 'aggdifxx', 'gdppc', 'polity2')])
for(i in 1:iters){ #with such a simple model, a for loop is fine, but parallelize for more complex models
  dataSub = dataComplete[sample(1:nrow(dataComplete), nrow(dataComplete), replace = T),]
  tempMod = glm(intercon ~ country_pop + aggdifxx + gdppc + polity2, 
                 data=dataSub,
                 family = binomial)
  coefs[i, ] = tempMod$coefficients
}
ests = apply(coefs, 2, mean)
stdError = apply(coefs, 2, sd)
summary(pop_logit)
cbind(ests, stdError)
cbind(ests, stdError, pop_logit$coefficients, summary(pop_logit)$coefficients[, 2])
