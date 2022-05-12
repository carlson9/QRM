setwd('~/QRM/inclass/day11TSCS/')
library(readstata13)
data = read.dta13('WCRWreplication.dta')

# TSCS and Panel Data Analyses

## One- and Two-Way Fixed Effects Models

simpleMod = lm(politychanget1 ~ nonviol, data = data)
summary(simpleMod)
#lets add a temporal effect - the beginning year seems most appropriate
timeMod = lm(politychanget1 ~ nonviol + as.factor(byear), #notice the as.factor wrapper - always use this to be safe so it is not interpretted numerically
             data = data)
summary(timeMod) #holds up
#we can make inferences about the fixed effects
plot(coef(timeMod)[-c(1:2)] ~ as.numeric(substr(names(coef(timeMod)[-c(1:2)]),
                                     start = 17, stop = 20)))
#doesnt actually seem that there is much of a trend in the baseline aggregate outcome
#now, lets add a location fixed effect
twowayMod = lm(politychanget1 ~ nonviol + as.factor(byear) + as.factor(location), #notice the as.factor wrapper - always use this to be safe so it is not interpretted numerically
             data = data)
summary(twowayMod) #still holds up, but keep in mind the very strong assumptions, especially when the fixed effects within country are not allowed to vary over time
#now lets look at a balanced example, which makes more intuitive and mathematical sense
#Bosnian ethnic voting as a function of violence experienced during the war
data2 = read.csv('bosnia.csv')
Log_Casualty.Model <- lm(Ethnic_Vote_Share ~ Log_Casualty,
                         data = data2)
summary(Log_Casualty.Model) #notice the lack of reliable effects
#lets add a province dummy
Log_Casualty.Model2 <- lm(Ethnic_Vote_Share ~ Log_Casualty + as.factor(Municipality),
                         data = data2)
summary(Log_Casualty.Model2) #still nothing, but notice the NAs - why?
#add temporal effects
Log_Casualty.Model3 <- lm(Ethnic_Vote_Share ~ Log_Casualty + as.factor(Municipality) + as.factor(Year),
                         data = data2)
summary(Log_Casualty.Model3) #still nothing
cbind(coef(Log_Casualty.Model)[2],
      coef(Log_Casualty.Model2)[2],
      coef(Log_Casualty.Model3)[2])
#the coefficient gets larger, even though there are very few degrees of freedom - why?
#we are no longer strictly controlling for heterogeneity, but exploring/exploiting province-year-level variation

## Difference-in-Difference Estimator

dummy.matrix <- as.matrix(cbind(as.numeric(data2$Year == 2006), as.numeric(data2$Year==2010), as.numeric(data2$Year==2014)))
Log_Casualty.Model.did <- lm(Ethnic_Vote_Share ~ Log_Casualty:dummy.matrix+dummy.matrix+
                   Municipality-1, data2) #why is there a minus 1?
summary(Log_Casualty.Model.did) #we see an effect, but decreasing in magnitude and reliability (keep this in mind)
summary(Log_Casualty.Model.did)$r.squared # R-squared: 0.9905888
#what's wrong with this R-squared?

## Lagged Dependent Variable (LDV) Models

## Correcting Standard Errors

library(sandwich)
library(multiwayvcov)
library(lmtest)
vcovCL1 <- cluster.vcov(Log_Casualty.Model.did, ~as.factor(data2$Municipality) + as.factor(data2$Year))
coeftest(Log_Casualty.Model.did, vcovCL1) #notice the standard errors actually decreased
#?cluster.vcov
#now lets try pcse for the first set of models
library(pcse)
data.1 = na.omit(data[, c('politychanget1',
                          'nonviol',
                          'byear',
                          'location')])
twowayMod = lm(politychanget1 ~ nonviol + as.factor(byear) + as.factor(location),
             data = data.1)
# vcovPC1 = pcse(twowayMod,
#      groupN = data.1$location,
#      groupT = data.1$byear,
#      pairwise = T) #takes some time
# coeftest(twowayMod, vcovPC1) #does not work - any idea why? think about how the data is assumed related to other data points

## Mixed (Random and Fixed) Effects

library(lme4)
REMod = lmer(politychanget1 ~ nonviol + (1|location),
             data = data.1)
summary(REMod) #if you look at the variation of random effects, location is not warranted
#now let's check time
REMod2 = lmer(politychanget1 ~ nonviol + (1|byear),
             data = data.1)
summary(REMod2) #no variation - no need for any heterogeneous exploration (though you might need to for reviewers, but you can argue these points preemptively or when you get reviews)

## Hausman Test

#only works for uncoupled data, e.g.:
library(plm)
# phtest(politychanget1 ~ nonviol,
#              data = data.1,
#        model = c("within", "random"),
#        index = c('byear', 'location'))
REmod = plm(Ethnic_Vote_Share ~ Log_Casualty,
                         data = data2,
            index = 'Year',
    model='random')
FEmod = plm(Ethnic_Vote_Share ~ Log_Casualty,
                         data = data2,
    index='Year',
    model='within')
phtest(FEmod, REmod)
#compare the models; why would we get this p-value?


