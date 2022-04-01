rm(list=ls())
setwd('~/QRM/inclass/day9extensions/')

#let's get some data from WDI
#let's model FDI inflows as function of GDPPC (race to the bottom?)

library(WDI)
WDIsearch('foreign direct investment')
# [4,] "BX.KLT.DINV.CD.WD"      "Foreign direct investment, net inflows (BoP, current US$)"                          
WDIsearch('gdp')
# [40,] "NY.GDP.PCAP.CD"             "GDP per capita (current US$)"                                                                                            
#let's control for natural resources (could easily affect both x and y)
WDIsearch('natural resources')
#[1,] "NY.GDP.TOTL.RT.ZS" "Total natural resources rents (% of GDP)"                                                   

data = WDI(country = 'all', indicator = c("BX.KLT.DINV.CD.WD", "NY.GDP.PCAP.CD", "NY.GDP.TOTL.RT.ZS"))

summary(data) #notice the NAs
#let's ignore and model
#first, notice the wildly different scales
#we are (for now) going to ignore year and country (there are other improvements to made as well)
dataScaled = na.omit(data)
dataScaled = apply(dataScaled[, 4:6], 2, scale)
colnames(dataScaled) = c('FDI', 'GDPPC', 'Resources')
dataScaled = as.data.frame(dataScaled)
modNaive = lm(FDI ~ ., dataScaled)
summary(modNaive)

library(finalfit)
colnames(data)[4:6] = c('FDI', 'GDPPC', 'Resources')
explanatory = c('GDPPC', 'Resources')
dependent = 'FDI'
data %>% ff_glimpse(dependent, explanatory)
data[, 4:6] %>% missing_plot()
data %>% missing_pattern(dependent, explanatory)
data %>% 
  summary_factorlist(dependent, explanatory, 
                     na_include=TRUE, p=TRUE)
data %>% 
  missing_pairs(dependent, explanatory)

data %>% 
  missing_compare(dependent, explanatory)

#sensitivity analysis
dataMinS = data
dataMinS$Resources[is.na(dataMinS$Resources)] = min(dataMinS$Resources, na.rm = T)
modWMin = lm(FDI ~ GDPPC + Resources, dataMinS)
summary(modWMin)

dataMaxS = data
dataMaxS$Resources[is.na(dataMaxS$Resources)] = max(data$Resources, na.rm = T)
modWMax = lm(FDI ~ GDPPC + Resources, dataMaxS)
summary(modWMax)

#mice
library(dplyr)
library(mice)

#drop rows with all NAs
dataS = data[, 4:6]
toDrop = apply(dataS, 1, function(x) all(is.na(x)))
data = data[!toDrop, ]

data %>% 
  select(all_of(dependent), all_of(explanatory)) %>% 
  missing_predictorMatrix(
    drop_from_imputed = 'FDI',
    drop_from_imputer = 'FDI'
  ) -> predM

fits = data %>% 
  select(dependent, explanatory) %>% 
  # Usually run imputation with 10 imputed sets (for publication becoming more common to use 100-1000), 4 here for demonstration
  mice(m = 4, predictorMatrix = predM) %>% 
  # Run lm regression on each imputed set
  with(lm(formula(ff_formula(dependent, explanatory))))

# Pool  results
fits_pool = fits %>% 
  pool()

# Summarise and put in table
fit_imputed = fits_pool %>%                                  
  fit2df(estimate_name = "LM (multiple imputation)")

