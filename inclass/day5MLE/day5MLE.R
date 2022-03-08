setwd('~/QRM/inclass/day5MLE/')
library(readstata13)
data = read.dta13('repdata.dta')

#binary outcome

#lets predict war as a function of Oil
#controls: "empgdpenl"  "emplpopl" "empolity2l" (notice the lags)

#logit (most common)
mod1 = glm(war ~ Oil + empgdpenl + emplpopl + empolity2l,
           data = data,
           family = binomial(link = 'logit'))
summary(mod1)

#probit
mod2 = glm(war ~ Oil + empgdpenl + emplpopl + empolity2l,
           data = data,
           family = binomial(link = 'probit'))
summary(mod2) #notice the different scale of the estimates

#c-log-log (very uncommon)
mod3 = glm(war ~ Oil + empgdpenl + emplpopl + empolity2l,
           data = data,
           family = binomial(link = 'cloglog'))
summary(mod3)

#counts

#lets predict number of wars as a function of democracy

#Poisson
mod4 = glm(wars ~ empolity2l + empgdpenl + emplpopl + ethfrac + relfrac,
           data = data,
           family = poisson(link = 'log'))
summary(mod4)

#negative binomial (for overdispersed)
library(MASS)
mod5 = glm.nb(wars ~ empolity2l + empgdpenl + emplpopl + ethfrac + relfrac,
           data = data) 
summary(mod5) #notice the warnings

mod5 = glm.nb(wars ~ empolity2l + empgdpenl + emplpopl + ethfrac + relfrac,
              data = data,
              control = glm.control(maxit = 20, epsilon = 1e-8)) 
summary(mod5)

#multiple categories

#lets predict region by ethnic frac

#logistic multinomial (using a neural net)
library(nnet)
mod6 = multinom(region ~ ethfrac + gdptype,
                data = data)
summary(mod6)
#logistic multinomial (MLE)
library(mlogit)
?mlogit #this library is a huge pain, so I suggest fitting with nnet
data("Fishing", package = "mlogit")
Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
summary(mlogit(mode ~ price + catch, data = Fish))

#ordered logistic regression
#just for illustration, well repeat the number of wars, but this is inappropriate because it is theoretically unbounded
library(MASS)
mod7 = polr(as.factor(wars) ~ empolity2l + empgdpenl + emplpopl + ethfrac + relfrac,
             data = data)
summary(mod7)
#as for multinomial and others, default is logit, but can be changed

#quasibinomial regression for proportions
#predict ethnic frac as a function of polity
mod8 = glm(ethfrac ~ empolity2l,
    data = data,
    family = quasibinomial)
summary(mod8)
