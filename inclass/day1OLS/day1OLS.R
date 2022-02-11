setwd('~/QRM/inclass/day1OLS/')

# Matrix Algebra
y = c(2,4,3,2) #vector
X = matrix(c(5,3,4,5,6,5,3,2,4,5,6,3,5,5,4,2),
           nrow = 4, ncol = 4, byrow = T) #matrix (you do not need to specify both cols and rows)
y
X
t(X) #transpose
solve(X) #inverse
solve(X) %*% X #identity
diag(1, 4) #identity
X = cbind(1, X) #add a column of 1's for the intercept
# solve(t(X) %*% X) %*% t(X) %*% y #linear model - why does it not work?
y = c(y, 6, 4.5, 5)
X = rbind(X, c(1, 2,6,4,3))
X = rbind(X, c(1, 3,4,5.5,2))
X = rbind(X, c(1, 4.6,7,3,2))
solve(t(X) %*% X) %*% t(X) %*% y #linear model
# lets put it in a function
linMod = function(X, y){
  beta = solve(t(X) %*% X) %*% t(X) %*% y
  se = sqrt(as.vector(t(y - X %*% beta) %*% (y - X %*% beta) / as.vector(nrow(X) - ncol(X))) * diag(solve(t(X) %*% X)))
  return(cbind(beta, se))
}
linMod(X, y)
summary(lm(y ~ X - 1)) # -1 means do not fit an intercept (there is a column of ones)
summary(lm(y ~ X[, -1])) # we could also drop the first column

library(readstata13)
data = readstata13::read.dta13('TamingGods.dta')
#explore
colnames(data)
head(data)
summary(data)

mod = lm(Religion ~ Ethnic, data = data)
summary(mod)
library(stargazer) #for making LaTex tables
stargazer(mod)
library(car)
qqPlot(mod) #check for error distribution - clearly not normal
plot(cooks.distance(mod)) #check for cook's influence - Cook's distance shows the influence of each observation on the fitted response values
par(mfrow = c(2,2)) #set up the plot to be 2x2 rows x columns
plot(mod) 
par(mfrow = c(1,1))
mod.null = lm(Religion ~ 1, data = data[!is.na(data$Ethnic),])
anova(mod, mod.null) #check the model against the null (typically just controls)

mod2 = lm(Religion ~ Ethnic*polity2_, data = data)
summary(mod2)
library(margins)
cplot(mod2, x = 'polity2_', what = 'effect', data = data)

mod3 = lm(Religion ~ Ethnic*polity2_ + I(polity2_ > 5), data = data)
cplot(mod3, x = 'polity2_', what = 'effect', data = data)
cplot(mod3, x = 'Ethnic', what = 'effect', data = data)
mod4 = lm(Religion ~ Ethnic*polity2_ + democracy, data = data)
cplot(mod4, x = 'polity2_', what = 'effect', data = data)
cplot(mod4, x = 'polity2_', what = 'prediction', data = data)

mod5 = lm(mpg ~ wt + I(wt^2), data = mtcars)
margins(mod5)
cplot(mod5, "wt", what = "prediction", main = "Predicted Fuel Economy, Given Weight")
cplot(mod5, "wt", what = "effect", main = "Average Marginal Effect of Weight")
mod6 = lm(mpg ~ hp * wt, data = mtcars)
persp(mod6, "wt", "hp", theta = c(45, 135, 225, 315), what = "effect")

