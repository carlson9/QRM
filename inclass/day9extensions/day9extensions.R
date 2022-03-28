rm(list=ls())
setwd('~/QRM/inclass/day9extensions/')
data = read.csv('../day6GLMs/data.csv')

# Quasi-Likelihood Estimation

pop_logit = glm(intercon ~ country_pop + aggdifxx + gdppc + polity2, 
                 data=data,
                 family = binomial)
summary(pop_logit) #notice (Dispersion parameter for binomial family taken to be 1)
q_pop_logit = glm(intercon ~ country_pop + aggdifxx + gdppc + polity2, 
                 data=data,
                 family = quasibinomial)
cbind(coef(pop_logit), coef(q_pop_logit)) #equivalent
cbind(confint(pop_logit), confint(q_pop_logit)) #suggests there is no problem
summary(q_pop_logit) #notice (Dispersion parameter for quasibinomial family taken to be 1.028087), so not really a problem here

#let's pretend polity2 is a count, and use gdppc to model it using a Poisson
pop_pois = glm(I(polity2 + 10) ~ gdppc, data = data,
               family = poisson)
summary(pop_pois) #(Dispersion parameter for poisson family taken to be 1)
q_pop_pois = glm(I(polity2 + 10) ~ gdppc, data = data,
               family = quasipoisson)
summary(q_pop_pois) #(Dispersion parameter for quasipoisson family taken to be 4.470922) != 1
cbind(confint(pop_pois), confint(q_pop_pois)) #larger CIs
#?family

#let's compare to negative binomial (for overdispersed counts)
library(MASS)
pop_nb = glm.nb(I(polity2 + 10) ~ gdppc, data = data)
cbind(coef(q_pop_pois), coef(pop_nb))
cbind(confint(q_pop_pois), confint(pop_nb))
#Since the negative binomial distribution has one more parameter than the Poisson, the second parameter can be used to adjust the variance independently of the mean
# In the case of modest overdispersion, this may produce substantially similar results to an overdispersed Poisson distribution

# Generalized Linear Mixed-Effects Model

library(lme4)
#start with a random intercept for year
hier_pop_logit = glmer(intercon ~ country_pop + 
                        aggdifxx + gdppc + polity2 +
                        (1|year), 
                 data=data,
                 family = binomial)
summary(hier_pop_logit) #so here we see that it is (near) singular, meaning we don't want to include this random effect as the variance is near zero
#perhaps the effect of country_pop varies by year
hier_pop_logit2 = glmer(intercon ~ 
                        aggdifxx + gdppc + polity2 +
                        (country_pop - 1|year), 
                 data=data,
                 family = binomial)
summary(hier_pop_logit2)
ranef(hier_pop_logit2)
plot(as.numeric(rownames(ranef(hier_pop_logit2)$year)), as.numeric(unlist(ranef(hier_pop_logit2)$year))) #effect is decreasing over time, with a spike after the Cold War, and one in 1979

# Fractional Regression

library(frm)
#let's convert polity2 to a proportion and model with gdppc
data2 = na.omit(data[, c('polity2', 'gdppc')])
pol2 = (data2$polity2+10)/20
gdppc = as.matrix(data2$gdppc)
colnames(gdppc) = 'gdppc'
mod_frac = frm(pol2, gdppc, linkfrac = 'logit')

#The Tobit Model

#polity is actually censored (and discrete, but we'll ignore that)
library(AER)
tob_mod = tobit(polity2 ~ gdppc, left = -10,
                right = 10, data = data)
summary(tob_mod)
#?tobit

# Zero-Inflated Models

#Zero inflated Poisson for rebellion data
library(pscl)
#?zeroinfl
pop_zinf = zeroinfl(rebellion ~ country_pop + aggdifxx + gdppc + polity2, 
                 data=data)
summary(pop_zinf)
#Zero inflated logit for intercon
library(Zelig)
pop_z = zelig(intercon ~ country_pop + 
                aggdifxx + gdppc + polity2, 
              model = 'logit',
                 data=data)
summary(pop_z) #this just drops zeros if needed, does not model two stages (need to move to writing your own Bayesian model)







