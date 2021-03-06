setwd('~/QRM/inclass/day2Assump/')

# Gauss-Markov assumptions

# - The model is linear in the parameters
# - No endogeneity in the model (independent variable $X$ and $\epsilon$ are not correlated)
# - Errors are normally distributed with constant variance
# - No autocorrelation in the errors
# - No multicollinearity between variable

## Linearity

# - The relationship between the predictor (x) and the outcome (y) is assumed to be linear
# - Non-linearity of the outcome - predictor relationships
# - Model plots:
# -- Residuals vs Fitted. Used to check the linear relationship assumptions. A horizontal line, without distinct patterns is an indication for a linear relationship, what is good.
# -- Normal Q-Q. Used to examine whether the residuals are normally distributed. It’s good if residuals points follow the straight dashed line.
# -- Scale-Location (or Spread-Location). Used to check the homogeneity of variance of the residuals (homoscedasticity). Horizontal line with equally spread points is a good indication of homoscedasticity.
# -- Residuals vs Leverage. Used to identify influential cases, that is extreme values that might influence the regression results when included or excluded from the analysis.

load('merged.Rdata') #load the data
head(merged)
mergedY = merged[!is.na(merged$avgAA),]
#model sentiment towards US as a function of inflation, with theoretical controls
mod = lm(avgAA ~ inflation + exports + imports + aid + propEmig, data = mergedY)
summary(mod)


# - To assess the assumption of linearity we want to ensure that the residuals are not too far away from 0 (standardized values less than -2 or greater than 2 are deemed problematic). To assess if the homoscedasticity assumption is met we look to make sure that there is no pattern in the residuals and that they are equally spread around the y = 0 line

plot(mod, 2) #is this linear?


# - Boxcox tranformation: Generic function used to compute the value(s) of an objective for one or more Box-Cox power transformations, or to compute an optimal power transformation based on a specified objective
# - Data transformations are often used to induce normality, homoscedasticity, and/or linearity

library(MASS)
boxcox(mod) #Box-Cox method only allows for strictly positive outcome

# - Box-Cox: If lambda does not equal zero, transform outcome to $\frac{y^\lambda - 1}{\lambda}$, if zero, take the log

mod2 = lm(I(log(avgAA)) ~ inflation + exports + imports + aid + propEmig, data = mergedY)
summary(mod)
summary(mod2)
#plot(mod2, 2) #need to create a new variable (I is not allowed in this function)
mergedY$y = log(mergedY$avgAA)
mod3 = lm(y ~ inflation + exports + imports + aid + propEmig, data = mergedY)
plot(mod3, 2) #still does not solve it - let's look at densities

vars = c('avgAA', 'inflation', 'exports', 'imports', 'aid', 'propEmig')
for(var in vars) plot(density(mergedY[,var]), main = var)

# - All of the independent variables are problematic, with long tails

summary(mergedY[, vars])

# - But, values are not strictly positive

mod4 = lm(y ~ I(log(inflation - min(inflation) + .01)) + I(log(exports - min(exports) + .01)) + I(log(imports - min(imports) + .01)) + I(log(aid - min(aid) + .01)) + I(log(propEmig - min(propEmig) + .01)), data = mergedY)
plot(mod4, 2)
boxcox(mod4)
summary(mod4)
#get it back to interpretable
coef(mod)[2]
#log(y) ~ log(X - c)*b
#exponentiate both sides
#y ~ (X - c)^b
ef = (mergedY$inflation - min(mergedY$inflation) + .01)^coef(mod4)[2]
plot(mergedY$inflation, ef) #diminshing effect
#what about uncertainty?
ef.lower = (mergedY$inflation - min(mergedY$inflation) + .01)^(coef(mod4)[2] - 1.96*coef(summary(mod4))[2, "Std. Error"])
ef.upper = (mergedY$inflation - min(mergedY$inflation) + .01)^(coef(mod4)[2] + 1.96*coef(summary(mod4))[2, "Std. Error"])
plot(mergedY$inflation, ef) #diminshing effect
segments(x0 = mergedY$inflation, y0 = ef.lower, x1 = mergedY$inflation, y1 = ef.upper)
library(margins)
x=cplot(mod4, x = 'inflation', what = 'effect') #what is the difference?
x[,2:4] = exp(x[,2:4])
plot(x[,1], x[,2])
segments(x0=x[,1], x1=x[,1], y0=x[,3], y1=x[,4])

# - What if we suspect a non-linear relationship and want to test for it?
# - We'll use the Boston data set [in MASS package], for predicting the median house value (mdev), in Boston Suburbs, based on the predictor variable lstat (percentage of lower status of the population)

#we'll use tidyverse this time
library(tidyverse)
library(caret)
theme_set(theme_classic())
# Load the data
data("Boston", package = "MASS")
# Split the data into training and test set
set.seed(123)
training.samples <- Boston$medv %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- Boston[training.samples, ]
test.data <- Boston[-training.samples, ]
ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth()
# Build the model
model <- lm(medv ~ lstat, data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)
ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x)
# Squaring
model2 = lm(medv ~ poly(lstat, 2, raw = TRUE), data = train.data)
# 6 degree polynomial
lm(medv ~ poly(lstat, 6, raw = TRUE), data = train.data) %>%
  summary()
# Drop the sixth
# Build the model
model3 <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data)
# Make predictions
predictions <- model3 %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)
ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))
# Log transformation
# Build the model
model4 <- lm(medv ~ log(lstat), data = train.data)
# Make predictions
predictions <- model4 %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)
ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ log(x))

# - Polynomial regression only captures a certain amount of curvature in a nonlinear relationship. An alternative, and often superior, approach to modeling nonlinear relationships is to use splines
# - Splines provide a way to smoothly interpolate between fixed points, called knots. Polynomial regression is computed between knots. In other words, splines are series of polynomial segments strung together, joining at knots
# - You need to specify two parameters: the degree of the polynomial and the location of the knots. In our example, we’ll place the knots at the lower quartile, the median quartile, and the upper quartile:

knots <- quantile(train.data$lstat, p = c(0.25, 0.5, 0.75))
library(splines)
# Build the model
knots <- quantile(train.data$lstat, p = c(0.25, 0.5, 0.75))
model <- lm (medv ~ bs(lstat, knots = knots), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)

# - Note that, the coefficients for a spline term are not interpretable

ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))

# - Once you have detected a non-linear relationship in your data, the polynomial terms may not be flexible enough to capture the relationship, and spline terms require specifying the knots.
# - Generalized additive models, or GAM, are a technique to automatically fit a spline regression. This can be done using the \texttt{mgvc} package:

library(mgcv)
# Build the model
model <- gam(medv ~ s(lstat), data = train.data)
# Make predictions
predictions <- model %>% predict(test.data)
# Model performance
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
)
ggplot(train.data, aes(lstat, medv) ) +
  geom_point() +
  stat_smooth(method = gam, formula = y ~ s(x))

# - From analyzing the RMSE and the R2 metrics of the different models, it can be seen that the polynomial regression, the spline regression and the generalized additive models outperform the linear regression model and the log transformation approaches

## Endogeneity

cor(mergedY[,'inflation'], summary(mod)$residuals) #we will deal with better tests later


## No autocorrelation in the errors

library(dynlm)
library(AER)
data("USMacroG")
mod.dyn = dynlm(consumption ~ dpi + L(dpi), data = USMacroG)
summary(mod.dyn)
durbinWatsonTest(mod.dyn)
durbinWatsonTest(mod.dyn, max.lag = 4)

library(itsadug)
mergedY = start_event(mergedY, column="year", event='pais', label.event="Event")
m1 <- bam(avgAA ~ te(year)+s(inflation), data = mergedY)
summary(m1)
acf(resid(m1))
acf(resid_gam(m1))
acf_resid(m1)

r1 <- start_value_rho(m1, plot=TRUE)
m1AR1 <- bam(avgAA ~ te(year)+s(inflation), data=mergedY, rho=r1, AR.start=mergedY$start.event)
summary(m1AR1)

acf_resid(m1)
acf_resid(m1AR1)

## No multicollinearity between variable

# - For a given predictor (p), multicollinearity can assessed by computing a score called the variance inflation factor (or VIF), which measures how much the variance of a regression coefficient is inflated due to multicollinearity in the model
# - The smallest possible value of VIF is one (absence of multicollinearity). As a rule of thumb, a VIF value that exceeds 5 or 10 indicates a problematic amount of collinearity
# - When faced to multicollinearity, the concerned variables should be removed, since the presence of multicollinearity implies that the information that this variable provides about the response is redundant in the presence of the other variables

vif(mod)
#exports and imports are very high
mod.vif = lm(avgAA ~ inflation + exports + aid + propEmig, 
    data = mergedY)
summary(mod.vif)


## Outliers

#Bonferroni outlier test
outlierTest(mod)
mod.out = lm(avgAA ~ inflation + aid + propEmig + exports + imports, mergedY[-81,])
summary(mod.out)


