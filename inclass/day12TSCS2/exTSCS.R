rm(list=ls())
setwd('~/QRM/inclass/day12TSCS2/')
load('TAPS2016.RData')

require(dplyr)
df = dataset %>% dplyr:: select(starts_with("APPRCONG"))

dd = apply(df, 2, function(x){
  as.numeric(as.character((recode_factor(x, `Refused` = 'NA',
         `Strongly Approve` = '5',
         `Somewhat Approve` = '4',
         `Not Sure` = '3',
         `Somewhat Disapprove` = '2',
         `Strongly Disapprove` = '1'))))
}) #for 'not sure' there is actually a heated discussion about what to do (and 'Refused' but less so)
#just for illustration purposes we will forward propogate NAs (often makes sense, and does here)

ca = as.data.frame(cbind(dataset$WUSTLID, dd))
#you can fairly easily do this in base R, but we'll try a package (there are many ways to do this)
library(reshape2)
require(ggplot2)

ca = melt(ca, id.vars = 'V1')
ca = ca[order(ca$V1), ]
require(tidyr)
ca = ca %>% fill(value)

#just in case
ca = na.omit(ca)

#spaghetti!
ggplot(data = ca, aes(x = variable, y = value, group = V1)) +
  geom_line(size=.1) + theme(legend.position="none")

#let's relate this to econ perceptions (hypotheses?)
df = dataset %>% dplyr:: select(starts_with("ECON2BS"))
df = apply(df, 2, function(x){
  as.numeric(as.character((recode_factor(x, `Refused` = 'NA',
                                         `excellent` = '5',
                                         `good` = '4',
                                         `not sure` = '3',
                                         `only fair` = '2',
                                         `poor` = '1'))))
})
df = as.data.frame(cbind(dataset$WUSTLID, df))

df = melt(df, id.vars = 'V1')
df = df %>% fill(value)

require(stringr)
ca$wave = as.numeric(str_sub(ca$variable, -2, -1))
df$wave = as.numeric(str_sub(df$variable, -2, -1))

toA = merge(ca, df, by = c('V1', 'wave'))
#lets rename things
colnames(toA)[c(1, 4, 6)] = c('ID', 'congApp', 'econPer')

#naive pooled approach
modNaive = lm(congApp ~ econPer, toA)
summary(modNaive) #obviously
#should we lag the IV? why or why not?

#lets exploit individual level variability
#FE for panelist (why not time?):
modFE = lm(congApp ~ econPer + as.factor(ID), toA)
summary(modFE)
#how about RE?
library(lme4)
modRE = lmer(congApp ~ econPer + (1 | ID), toA) #notice how much faster; any idea why?
summary(modRE) #still no results

#let's explore party id
pp = dataset %>% dplyr:: select("PARTYID1S50") #first wave is sufficient
pp = as.data.frame(cbind(dataset$WUSTLID, pp))
colnames(pp)[1] = 'ID'
toA = merge(pp, toA, by = 'ID', all = T)
colnames(toA)[2] = 'party'

#FE with varying slopes by party
modFE2 = lm(congApp ~ econPer*party, toA)
summary(modFE2) #repubs are most affected, dems are least, unsurprising

#RE approach
modRE2 = lmer(congApp ~ econPer + (econPer + 1 | party) - 1, toA)
summary(modRE2)
ranef(modRE2)

require(lmtest)
require(lmerTest)
bm = as_lmerModLmerTest(modRE2)
#?ranova
#understand this
ranova(bm)
