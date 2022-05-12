rm(list=ls())
setwd('~/QRM/inclass/day10temporal/')

#let's start with a continuous (unbounded) outcome for intuition
#we'll move on to others below

#note that 'true' unboundedness is very rare in political outcomes, but upon transformations, we can treat them as unbounded
#let's grab refugee population in Turkey from 1960-2020
library(WDI)
refPop = WDI(country = 'TR', indicator = 'SM.POP.REFG')
#what is far more relevant is the proportion of the population as refugees
#we'll need total pop as well
turkeyPop = read.csv('turkeyPop.csv', header = F)
#a couple things to notice
  #the second column is ill-formatted; need to adjust
  #we will need to interpolate (not a big deal for pop)

colnames(turkeyPop) = c('year', 'pop')
popToFix = turkeyPop$pop
turkeyPop$pop = as.numeric(gsub(' ', '', unlist(lapply(popToFix, as.character))))

#formatting is fixed, now to interpolate
#let's plot to get a sense
plot(turkeyPop$year, turkeyPop$pop)
#looks incredibly linear - no need for splines, let's just use linear interpolation
#first, let's merge the data
data = merge(refPop, turkeyPop, by = 'year', all = T)
library(zoo)

#this is already in the correct order, but in case it's not:
data = data[order(data$year), ]
data$pop = na.approx(data$pop)
#let's plot to get a sense again
plot(data$year, data$pop) #seems reasonable enough
#now let's get proportions
data$propRef = data$SM.POP.REFG/data$pop
#2021 not in refugee data, so drop
data = data[data$year != 2021, ] #many ways to do this, but try to be robust to not make mistakes
#let's get a sense of the dist
plot(density(data$propRef)) #problematic, but let's look at the temporal variation
plot(data$year, data$propRef)
#obviously, after 2011 huge spike; we don't need a model to tell us this (or why)
#let's drop after 2011 and think about other patterns
data = data[data$year <= 2011, ] #again, many ways
#plot again
plot(data$year, data$propRef)
#now we see more interesting variation
#let's look at another density plot
plot(density(data$propRef))
#now, transform to be continuous
#first, log
data$propRefLog = log(data$propRef)
plot(density(data$propRefLog))
#now, scale
data$propRefLogSc = scale(data$propRefLog)
plot(density(data$propRefLogSc))
#over time
plot(data$year, data$propRefLogSc)
#interesting, and now unbounded and continuous

#what could explain this, other than exogenous shocks?
#"DT.ODA.ALLD.CD" "Net official development assistance and official aid received (current US$)"
monetaryData = WDI(country = 'TR', end = 2011, indicator = 'DT.ODA.ALLD.CD')
#go ahead and scale
monetaryData$aidSc = scale(monetaryData$DT.ODA.ALLD.CD)
data = merge(data, monetaryData, by = 'year')
#overlay a plot of aid
points(data$year, data$aidSc, pch = '+')
#definitely seems like a correlation
#let's naively test
mod = lm(propRefLogSc ~ aidSc, data)
summary(mod)
#we could actually drop the intercept (notice it's close to zero) because both are scaled
#we will leave it in though, because we ultimately care about temporality
#we could frame two hypotheses:
  #refugees increase with aid
  #aid increases refugee admittance
#pick the hypothesis, and lag the explanatory variable
#I imagine (and looking at the plot think) that aid is a function of refugee admittance, not the other way
#we therefore lag refugee prop
library(Hmisc)
data$propRefLogScL1 = Lag(data$propRefLogSc, +1)
#now model
modL1 = lm(aidSc ~ propRefLogScL1, data)
summary(modL1)

#but what if we hypothesized the other direction?
data$aidScL1 = Lag(data$aidSc, +1)
#now model
mod2L1 = lm(propRefLogSc ~ aidScL1, data)
summary(mod2L1)
#ok, so which one is right? who the hell knows

#I'm saving the data in case the indicators change (good practice)
saveRDS(data, 'refAid.rds')

#now let's check assumptions of first model
acf(data$aidSc) #problematic
pacf(data$aidSc) #suggests AR(I)MA - but let's wait
library(tseries)
#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
kpss.test(data$aidSc, null = 'Level') #hmm...
#(Augmented) Dickey Fuller test (more common, but simply worse)
adf.test(data$aidSc, alternative = 'stationary', k=0) #problematic
adf.test(data$aidSc, alternative = 'explosive', k=0) #problematic
#instead of exploring lags, let's use auto.arima()
#Returns best ARIMA model according to either AIC, AICc or BIC value. The function conducts a search over possible model within the order constraints provided.
library(forecast)
auto.arima(data$aidSc)
#ok, we should include a lagged DV as well
modARIMA = lm(aidSc ~ propRefLogScL1 + aidScL1, data)
summary(modARIMA) #so, how to interpret?
#what about instantaneous and lagged IV?
modWTF = lm(aidSc ~ propRefLogSc + propRefLogScL1, data)
summary(modWTF)
#considering N, these are significant results
#but how do we interpret this?
#mostly useful for forecasting

#even more 'robust' (but useless for inference, almost)
modWTF2 = lm(aidSc ~ propRefLogSc + propRefLogScL1 + aidScL1, data)
summary(modWTF2) #results are confusing at best

#let's omit the unlagged IV
modWTF3 = lm(aidSc ~ propRefLogScL1 + aidScL1, data)
summary(modWTF3) #insignificant, but again, what the hell does this mean?

#return to the original L1 model
summary(modL1)
#let's adjust the standard errors based on temporality (Newey West - I have written about SE adjustments; they are common, but not terribly good)
library(sandwich)
library(lmtest)
#we need to drop NAs manually
modL1data = na.omit(data.frame('aidSc' = data$aidSc, 'propRefLogScL1' = data$propRefLogScL1, 'year' = data$year))
modL1x = lm(aidSc ~ propRefLogScL1, modL1data)
coeftest(modL1x,
         vcov. = NeweyWest(modL1x,
                           order.by = ~modL1data$year))
#same estimate, but MUCH wider SE and insignificant

#as said, teasing out causality in TS data is hard
#I have a (very complicated) paper on it



#now let's analyze journalist imprisonments in Turkey before and after the recent coup attempt
#this is much more likely the type of hypotheses you'll be testing in TS
#now bounded and discrete data in the outcome
# Scraping
library("rvest")
# Melting
library("data.table")
# Piping
library("magrittr")
# Manipulating
library("dplyr")
# Plotting
library("ggplot2")
# Styling
library("ggthemes")

url <- "https://en.wikipedia.org/wiki/List_of_arrested_journalists_in_Turkey"
kept_columns <- c("Date arrested")
webpage <- read_html(url)
tbls <- html_nodes(webpage, "table")
# Shows all tables
tbls
# If more matched, find search term
tbls[grep("Arrested",tbls,ignore.case = T)]
df <- html_table(tbls[grep("Arrested",tbls,ignore.case = T)],fill = T)[[1]]
arrestDate = df[, kept_columns]
arrestDate = sub("\\[.*", "", arrestDate)
arrestDateHold = as.Date(arrestDate)
arrestDateHold[is.na(arrestDateHold)] = c('2009-03-24', '', '', '', '', '', '2017-06-14', '2016-10-01', '', '2017-04-01', '2017-03-01', '')
arrestDate = as.Date(arrestDateHold)
arrestYear = year(arrestDate)
arrestYear = arrestYear[arrestYear >= 1994]
years = as.numeric(names(table(arrestYear)))
freqY = table(arrestYear)
plot(years, freqY)
abline(v = 2015.5, lty = 2)
#was 2016 a statistical anamoly?
#let's start with agnostically looking for changepoints
#segmented
library(segmented)
fit_lm = lm(freqY ~ 1 + years)  # intercept-only model
fit_segmented_list = lapply(1:5, function(bp) segmented(fit_lm, seg.Z = ~years, npsi = bp))  # 1:5 change points along x

lapply(fit_segmented_list, summary)
lapply(fit_segmented_list, BIC)
#2 cut-offs seems appropriate
plot(fit_segmented_list[[2]])
points(years, freqY)
lines.segmented(fit_segmented_list[[2]])
points.segmented(fit_segmented_list[[2]])
#so despite the previous peak, we still find relatively strong evidence while being agnostic that there was a change in 2016

#what about a link function? we'll use Poisson
fit_p = glm(freqY ~ 1 + years, family = 'poisson')  # intercept-only model
fit_segmented_list = lapply(1:5, function(bp) segmented(fit_p, seg.Z = ~years, npsi = bp))  # 1:5 change points along x

lapply(fit_segmented_list, summary)
lapply(fit_segmented_list, BIC)
#now 2 cut-offs still seems appropriate (because the third could not estimate 3 cut-points)
plot(fit_segmented_list[[2]])
points(years, freqY)
lines.segmented(fit_segmented_list[[2]])
points.segmented(fit_segmented_list[[2]])


#same, but a slightly different library that optimizes fit stats for us
library(strucchange)
fit_bp = breakpoints(freqY ~ 1, breaks = 5)
summary(fit_bp)
#this now recommends 0 breakpoints, which we did not even consider, suggesting the peaks are not anomylous

#bcp: Bayesian; automatically detects change points and segment types, though you can use the parameter d to increase the prior probability of intercept-only models
# It provides estimates of means and probability of change point at each x-coordinate. 
library(bcp)
freqYdata = data.frame('freqY' = freqY, 'years' = years)
fit_bcp = bcp(freqYdata$freqY.Freq, d = 1000)
plot(fit_bcp)
#suggests 3 cut-off points, 2 massive peaks
#problems are not directly modeling trends

#more implentations with different features: https://lindeloev.github.io/mcp/articles/packages.html
