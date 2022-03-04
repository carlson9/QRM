# More advanced plotting, introductory simulations

setwd('~/QRM/inclass/day4Exp')

## Simulate TSCS data

# lets create a function to generate one tscs sample with auto-correlation
tscsGen = function(rho, n.groups = 15, n.obs = 15, seed = 1523){
  set.seed(seed)
  returnMat = expand.grid(1:n.groups, 1:n.obs)
  colnames(returnMat) = c('Group', 'Time')
  returnMat$x = NA
  returnMat$x[returnMat$Time == 1] = rnorm(n.groups)
  err = numeric(n.obs*n.groups)
  err[returnMat$Time == 1] = rnorm(n.groups)
  
  for(g in 1:n.groups){
    for(t in 2:n.obs){
      returnMat$x[returnMat$Group == g & returnMat$Time == t] = rho * returnMat$x[returnMat$Group == g & returnMat$Time == (t - 1)] + sqrt(1 - rho^2)*rnorm(1)
      err[returnMat$Group == g & returnMat$Time == t] = rho * err[returnMat$Group == g & returnMat$Time == (t - 1)] + sqrt(1 - rho^2)*rnorm(1)
    }
  }
  err = scale(err)
  g.ef = rnorm(n.groups)
  returnMat$y = -1 + rep(g.ef, times = n.groups) + .4*returnMat$x + err
  return(returnMat)
}

## Generate one TSCS dataset at $\rho = 0.8$ and plot the variable relationships

data = tscsGen(rho = .8)
#the goal is to plot every trend in x on one window
plot(data$x[data$Group == 1] ~ unique(data$Time), type = 'l')
#add axis titles
plot(data$x[data$Group == 1] ~ unique(data$Time), type = 'l',
     xlab = 'Time', ylab = 'x')
#lets pretty it up
par(mfrow=c(1,1), mgp=c(1,0,0), tcl=0, mar=c(2,2,1,1), cex.lab=1.2, cex.axis=1.1)
plot(data$x[data$Group == 1] ~ unique(data$Time), type = 'l',
     xlab = 'Time', ylab = 'x')
#well need to add other lines, and have to change the dim of the plot window accordingly
plot(data$x[data$Group == 1] ~ unique(data$Time), type = 'l',
     xlab = 'Time', ylab = 'x', ylim = c(min(data$x)-.1, max(data$x) +.1))
#lets add color to separate the lines, and add lines
library(viridis)
col = viridis(15)
plot(data$x[data$Group == 1] ~ unique(data$Time), type = 'l',
     xlab = 'Time', ylab = 'x', ylim = c(min(data$x)-.1, max(data$x) +.1),
     col = col[1])
for(i in 2:max(data$Group)) points(data$x[data$Group == i] ~ unique(data$Time), col = col[i], type = 'l')
#lets increase the size of the window to fit a legend
plot(data$x[data$Group == 1] ~ unique(data$Time), type = 'l',
     xlab = 'Time', ylab = 'x', ylim = c(min(data$x)-.1, max(data$x) +1),
     col = col[1])
for(i in 2:max(data$Group)) points(data$x[data$Group == i] ~ unique(data$Time), col = col[i], type = 'l')
legend('topleft', legend = 1:max(data$Group), col = col, lty = 1, cex = .3, bty = 'n')
#what about line types for greyscale?
plot(data$x[data$Group == 1] ~ unique(data$Time), type = 'l',
     xlab = 'Time', ylab = 'x', ylim = c(min(data$x)-.1, max(data$x) +1),
     col = col[1])
for(i in 2:max(data$Group)) points(data$x[data$Group == i] ~ unique(data$Time), col = col[i], type = 'l', lty = i)
legend('topleft', legend = 1:max(data$Group), col = col, lty = 1:max(data$Group), cex = .3, bty = 'n', horiz = T)
#notice the recycling of lty - lets add points as well
plot(data$x[data$Group == 1] ~ unique(data$Time), type = 'b',
     xlab = 'Time', ylab = 'x', ylim = c(min(data$x)-.1, max(data$x) +1),
     col = col[1], pch = 1, cex = .3)
for(i in 2:max(data$Group)) points(data$x[data$Group == i] ~ unique(data$Time), col = col[i], type = 'b', lty = i, pch = i, cex = .3)
legend('topleft', legend = 1:max(data$Group), col = col, lty = 1:max(data$Group), pch = 1:max(data$Group), cex = .3, bty = 'n', horiz = T)
#lets repeat for y to investigate the auto-correlation in the outcome
plot(data$y[data$Group == 1] ~ unique(data$Time), type = 'b',
     xlab = 'Time', ylab = 'y', ylim = c(min(data$y)-.1, max(data$y) +1),
     col = col[1], pch = 1, cex = .3)
for(i in 2:max(data$Group)) points(data$y[data$Group == i] ~ unique(data$Time), col = col[i], type = 'b', lty = i, pch = i, cex = .3)
legend('topleft', legend = 1:max(data$Group), col = col, lty = 1:max(data$Group), pch = 1:max(data$Group), cex = .3, bty = 'n', horiz = T)
#notice there are also different baselines
#now lets plot y as a function of x, first all points
plot(data$y ~ data$x, type = 'p',
     xlab = 'x', ylab = 'x', cex = .3)
#add a line of best fit
abline(lm(y ~ x, data))
#now lets allow different baselines
#notice the as.factor
mod = lm(y ~ x + as.factor(Group) - 1, data)
plot(data$y[data$Group == 1] ~ data$x[data$Group == 1], type = 'p',
     xlab = 'x', ylab = 'y', ylim = c(min(data$y)-.1, max(data$y) +1),
     xlim = c(min(data$x)-.1, max(data$x) +.1),
     col = col[1], pch = 1, cex = .3)
for(i in 2:max(data$Group)) points(data$y[data$Group == i] ~ data$x[data$Group == i], col = col[i], type = 'p', lty = i, pch = i, cex = .3)
legend('topleft', legend = 1:max(data$Group), col = col,
       pch = 1:max(data$Group), cex = .3, bty = 'n', horiz = T)
#add the lines
for(i in 2:max(data$Group)) abline(a = coef(mod)[i+1], b = coef(mod)[1], col = col[i])
#now lets use ggplot2
data$pred = predict(mod)
library(ggplot2)
ggplot(data, aes(x = x, y = y, color = as.factor(Group)) ) +
     geom_point() +
     geom_line(aes(y = pred), size = 1)
#lets add confidence intervals
predslm = predict(mod, interval = "confidence")
data = cbind(data, predslm)
ggplot(data, aes(x = x, y = y, color = as.factor(Group)) ) +
     geom_point() +
     geom_ribbon( aes(ymin = lwr, ymax = upr, fill = as.factor(Group), color = NULL), alpha = .15) +
     geom_line( aes(y = fit), size = 1)

## Analyze the generated TSCS data and plot the results

#we want to simulate a bunch of data sets at a range of rho, run a linear model, and check for coverage and false negatives
simAnalysis = function(rho = seq(0, .9, by = .1),
                       n.datasets = 1000, ...){ #the ... notation allows additional arguments to be sent to lower level functions
  datas = lapply(1:n.datasets, function(y){
    lapply(rho, function(x){
      dd = tscsGen(x, seed = 100*y + 10*x, ...)
      mod = lm(y ~ x + as.factor(Group) - 1, dd)
      cov = confint(mod)[1,1] < .4 & confint(mod)[1,2] > .4
      pos = confint(mod)[1,1] > 0
      return(list(cov, pos))
  })})
  datas = unlist(datas)
  #so, it looped through each rho nested within each dataset
  datas.cov = datas[seq(1, length(datas), by = 2)]
  datas.pos = datas[seq(2, length(datas), by = 2)]
  covsByRho = numeric(length(rho))
  possByRho = numeric(length(rho))
  for(i in 1:length(rho)){
    covsByRho[i] = mean(datas.cov[seq(i, length(datas.cov), by = length(rho))])
    possByRho[i] = mean(datas.pos[seq(i, length(datas.pos), by = length(rho))])
  }
  return(list(covsByRho, possByRho))
} #hw - parallelize this code
toAnalyze = simAnalysis()
save(toAnalyze, file = 'sims.Rdata')

load('sims.Rdata')
plot(toAnalyze[[1]] ~ seq(0, .9, by=.1), ylim = c(0,1), type = 'l',
     xlab = expression(rho), ylab = 'True Rate')
points(toAnalyze[[2]] ~ seq(0, .9, by=.1), type='l', lty = 2)

## Coefficient plots

set.seed(99)
X = matrix(rnorm(1000*3), ncol = 3, nrow = 1000)
y = -1.5 + X%*%c(.5, -1.2, .3) + rnorm(1000)
mod = lm(y ~ X)
coef(mod)
confint(mod)
plot(coef(mod), 1:4, yaxt = 'n', pch = 18,
     xlim = c(min(confint(mod)), max(confint(mod))),
     ylab = '', xlab = 'Estimates')
segments(x0 = confint(mod)[,1], x1 = confint(mod)[,2],
         y0 = 1:4, y1 = 1:4)
abline(v=0, lty = 3)
axis(2, at = 1:4,
     tick = F, labels = c(expression(beta['0']),
                          expression(beta['1']),
                          expression(beta['2']),
                          expression(beta['3'])),
     las = 1)
library(broom)
coef = tidy(mod, conf.int = T)
coef
ggplot(coef, aes(term, estimate))+
  geom_point()+
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high))+
  labs(title = "Coefficients of a linear regression model")


