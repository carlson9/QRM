rm(list=ls())
if(dirname(rstudioapi::getActiveDocumentContext()$path) != '') setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

final_df = read.csv('data.csv')

#running a simple regression on intercommunal violence (not violence with the state):
pop_logit <- glm(intercon ~ country_pop + aggdifxx + gdppc + polity2, 
                 data=final_df,
                 family = binomial)
summary(pop_logit)
#lets get a tex table
library(stargazer)
stargazer(pop_logit)

#we might prefer Word or HTML
library(jtools)
export_summs(pop_logit, to.file = 'docx')

#exponentiated coefficients
export_summs(pop_logit, exp = T)

#plot summarization
plot_summs(pop_logit)

#exponentiate
plot_summs(pop_logit, exp = T)

#plot predictions

#let's get predicted probabilities with SEs
probs = predict(pop_logit, type = 'response', se.fit = T)
#add uncertainty
lowers = probs$fit - 1.96*probs$se.fit
uppers = probs$fit + 1.96*probs$se.fit

newData = na.omit(final_df[, c('intercon', 'country_pop', 'aggdifxx', 'gdppc', 'polity2')])

plot(probs$fit ~ newData$country_pop, pch = 18)
segments(x0 = newData$country_pop, x1 = newData$country_pop,
         y0 = lowers, y1 = uppers)

#very ugly, and not that meaningful
#instead, take the mean of other variables, and allow country_pop to vary
dataToPlot = data.frame('country_pop' = seq(min(newData$country_pop), max(newData$country_pop), length.out = 1000),
                        'aggdifxx' = mean(newData$aggdifxx),
                        'gdppc' = mean(newData$gdppc),
                        'polity2' = mean(newData$polity2))
probs = predict(pop_logit, newdata = dataToPlot, type = 'response', se.fit = T)
lowers = probs$fit - 1.96*probs$se.fit
uppers = probs$fit + 1.96*probs$se.fit
plot(probs$fit ~ dataToPlot$country_pop, type = 'n')
points(dataToPlot$country_pop, lowers, lty = 2, type = 'l')
points(dataToPlot$country_pop, uppers, lty = 2, type = 'l')
polygon(c(rev(dataToPlot$country_pop), dataToPlot$country_pop), c(rev(uppers), lowers), col = 'grey80', border = NA)
points(probs$fit ~ dataToPlot$country_pop, type = 'l')
#add a rug if you want
rug(newData$country_pop)

#in papers, you generally want an intuitive numerical explanation
#not that important for logits, cause of exp, but for GLMs in general
#we take the mean prediction, and subtract the sd
diff(predict(pop_logit, newdata = data.frame(
  'country_pop' = c(mean(newData$country_pop), mean(newData$country_pop) - sd(newData$country_pop)),
  'aggdifxx' = mean(newData$aggdifxx),
  'gdppc' = mean(newData$gdppc),
  'polity2' = mean(newData$polity2)
), type = 'response'))
#-0.1248939

#or we might report the max to min
diff(predict(pop_logit, newdata = data.frame(
  'country_pop' = c(max(newData$country_pop), min(newData$country_pop)),
  'aggdifxx' = mean(newData$aggdifxx),
  'gdppc' = mean(newData$gdppc),
  'polity2' = mean(newData$polity2)
), type = 'response'))
#-0.438561

#now matched cases
library(MatchIt)
match.out = matchit(I(country_pop > mean(country_pop)) ~ 
                                 aggdifxx + gdppc + polity2,
        data = newData,
        method = 'nearest', distance = 'mahalanobis',
        replace = T)
matches = as.numeric(match.out$match.matrix)
matches2 = as.numeric(row.names(match.out$match.matrix))
mean(final_df[matches2, 'intercon'] - final_df[matches, 'intercon'])
final_df[matches2[1], 'intercon'] - final_df[matches[1], 'intercon'] #way more meaningful when it's not binary

#now we compare to the null model to check for explanatory power
pop_logitNull <- glm(intercon ~ aggdifxx + gdppc + polity2, 
                 data=final_df,
                 family = binomial)

#analysis of variance
#we use a chi-squared test here
anova(pop_logit, pop_logitNull, test = 'Chisq')
#highly significant

#two models in one call
stargazer(pop_logit, pop_logitNull)






con_logit <- glm(intercon ~ groupcon + aggdifxx + gdppc + polity2, 
                 data=final_df,
                 family = binomial)
summary(con_logit)

anova(pop_logit, con_logit)
#model 1 is better

binary_err_rate = function(mod.formula, data, seed = 1234, train = .8, ndraws = 1000){
  set.seed(seed)
  MSE = c()
  for(i in 1:ndraws){ #can parallelize if extra cores are available
    samp = sample(1:nrow(data), nrow(data)*train)
    train_data = data[samp,]
    test_data = data[-samp,]
    mod = glm(mod.formula, data = train_data,   
              family = binomial(link = 'logit'))
    probs = predict(mod, test_data, type = 'response')
    MSE = c(MSE, mean((probs - (as.numeric(test_data[, as.character(mod.formula[2])]) - 1))^2, na.rm = T))
  }
  return(mean(MSE))
}

binary_err_rate(mod.formula = pop_logit$formula, data = final_df)
#1.233417
binary_err_rate(mod.formula = con_logit$formula, data = final_df)
#1.235093
#model 1 is better

#now running the ordered logit regression on rebellion score
library(MASS)
pop_olr <- polr(as.factor(rebellion) ~ country_pop + aggdifxx + gdppc + polity2, 
                data=final_df)
summary(pop_olr)

con_olr <- polr(as.factor(rebellion) ~ groupcon + aggdifxx + gdppc + polity2, 
                data=final_df)
summary(con_olr)

anova(pop_olr, con_olr)
#model 2 is better

#again, compare to null

pop_olrNull <- polr(as.factor(rebellion) ~ aggdifxx + gdppc + polity2, 
                data=final_df)
anova(pop_olr, pop_olrNull)
#highly significant


#plot the results of a polr
newData = na.omit(final_df[, c('rebellion', 'country_pop', 'aggdifxx', 'gdppc', 'polity2')])
probs = as.data.frame(predict(pop_olr, newData, se.fit = TRUE, type = 'probs'))
plot(0:8, probs[1,], type = 'b', pch = 18)
for(i in 2:nrow(newData)) points(0:8, probs[i,], type = 'b', pch = 18)
#now work with the same as above to make a more meaningful plot

dataToPlot = data.frame('country_pop' = seq(min(newData$country_pop), max(newData$country_pop), length.out = 1000),
                        'aggdifxx' = mean(newData$aggdifxx),
                        'gdppc' = mean(newData$gdppc),
                        'polity2' = mean(newData$polity2))
probs = predict(pop_olr, newdata = dataToPlot, type = 'probs')

library(viridis)
cols = viridis(9)
plot(probs[,1] ~ dataToPlot$country_pop, type = 'l', col = cols[1], ylim = c(0,.9))
for(i in 2:9) points(dataToPlot$country_pop, probs[,i], col = cols[i], type = 'l')

#add a rug if you want
rug(newData$country_pop)
#add a legend
legend('topright', col = cols, legend = 0:8, lty = 1)


#######################


polr_toMax = function(expl, data, seed = 1234, train = .8, ndraws = 1000){
  set.seed(seed)
  toMax = c()
  require(MASS)
  for(i in 1:ndraws){ #can parallelize if extra cores are available
    samp = sample(1:nrow(data), nrow(data)*train)
    train_data = data[samp,]
    test_data = data[-samp,]
    mod = polr(as.factor(rebellion) ~ get(expl) + aggdifxx + gdppc + polity2, 
               data=train_data)
    probs = predict(mod, test_data, type = 'probs')
    probSum = sum(unlist(lapply(1:nrow(test_data), function(x){
      probs[x, test_data[x, 'rebellion']]
    })), na.rm = T)
    toMax = c(toMax, probSum)
  }
  return(mean(toMax))
}

polr_toMax(expl = 'country_pop', data = final_df)
#104.8781
polr_toMax(expl = 'groupcon', data = final_df)
#102.7683
