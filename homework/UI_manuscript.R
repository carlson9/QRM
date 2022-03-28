#######################
# set working directory
# load data
# and load libraries
#######################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
  sink()
}

lapply(c("ggplot2", "texreg", "Zelig", "margins", "tidyr", "dplyr"), pkgTest)

# set working directory to parent replication folder
# this shouldn't be impacted where you downloaded 
# the replication files
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load data
finalData <- read.csv("../data/finalData.csv", stringsAsFactors = T)
finalData$NY.GDP.MKTP.CD <- log(finalData$NY.GDP.MKTP.CD)
finalData <- finalData[, c("year", "country1", "country2", "polity2", "polconiii", "ICSID",
                           "NY.GDP.MKTP.CD","NY.GDP.MKTP.KD.ZG", "NE.TRD.GNFS.ZS","BX.KLT.DINV.WD.GD.ZS","NY.GDP.TOTL.RT.ZS",
                           "timeUntilAnyElec", "demoBin", "anyViolations", "violations", "treatyExistence")]
############
# Figure 1
############

finalDataCompleteCases <- finalData %>% drop_na()
levels(finalDataCompleteCases$demoBin) <- c("Autocracy", "Anocracy", "Democracy")
finalDataCompleteCases$violations <- ifelse(is.na(finalDataCompleteCases$violations), 0, finalDataCompleteCases$violations)

sign_count <- finalDataCompleteCases %>%
  group_by(year, demoBin) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
sign_count$freq <- sign_count$freq*100

### left panel 

pdf('../figures/fig1A.pdf', width=8)
ggplot(sign_count, aes(year, freq, colour=demoBin, shape=demoBin, lty=demoBin))+
  geom_point(size=4) + geom_line(size=1.5)+
  scale_colour_grey(start = 0.6, end = 0.2)  + 
  lims(x=c(1995, 2013), y=c(0,100))+
  labs(x="Year", y="Percent of all treaties in\nforce for a given year", shape= "Level of Democracy", 
       colour = "Level of Democracy", lty="Level of Democracy") + 
  theme_classic() + theme(legend.position = c(0.75, 0.85), legend.text=element_text(size=20),
                          legend.title=element_text(size=25), 
                          legend.background = element_rect(fill="white",size=0.5, linetype="solid", colour ="black"),
                          axis.text=element_text(size=24), axis.title=element_text(size=28))
dev.off()

violation_count <- merge(sign_count, finalDataCompleteCases[!is.na(finalDataCompleteCases$demoBin), ] %>%
                   group_by(year, demoBin) %>%
                   summarise(violations = sum(violations)), by=c("year", "demoBin"), all.x = T)
violation_count$freq2 <- (violation_count$violations/violation_count$n)

### right panel

pdf('../figures/fig1B.pdf', width=8)
ggplot(violation_count, aes(year, freq2, colour=demoBin, shape=demoBin, lty=demoBin))+
  geom_point(size=4) + geom_line(size=1.5)+
  scale_colour_grey(start = 0.6, end = 0.2)  + 
  lims(x=c(1995, 2013))+
  labs(x="Year", y="Percent of all in force treaties that\nwere violated for a given year", shape= "Level of democracy", colour = "Level of democracy") + 
  theme_classic() + theme(legend.position = "none", legend.text=element_text(size=20),
                          legend.title=element_text(size=25), legend.background = element_rect(fill="white", size=0.5, linetype="solid", colour ="black"),
                          axis.text=element_text(size=24), axis.title=element_text(size=28))
dev.off()

############
# Figure 2
############

### logit
logitFull <- glm(anyViolations ~ polity2*timeUntilAnyElec+ polconiii + log(NY.GDP.MKTP.CD)+ NY.GDP.MKTP.KD.ZG+
                   NE.TRD.GNFS.ZS + BX.KLT.DINV.WD.GD.ZS +
                   NY.GDP.TOTL.RT.ZS,
                 data=finalData, family = "binomial")

### Descriptions in text
# calculate odds ratio p. 13
exp(cbind(coef(logitFull), confint(logitFull)))  

# marginal shifts described on p. 12
logitZFull <- zelig(anyViolations ~ polity2*timeUntilAnyElec+ polconiii + log(NY.GDP.MKTP.CD)+ NY.GDP.MKTP.KD.ZG+
                      NE.TRD.GNFS.ZS + BX.KLT.DINV.WD.GD.ZS +
                      NY.GDP.TOTL.RT.ZS,
                    data=finalData, model = "logit")

sim(logitZFull, x = setx(logitZFull, polity2 = -3, timeUntilAnyElec = -4), x1 = setx(logitZFull, polity2 = -3, timeUntilAnyElec = 0), num = 10000)
sim(logitZFull, x = setx(logitZFull, polity2 = 10, timeUntilAnyElec = -4), x1 = setx(logitZFull, polity2 = 10, timeUntilAnyElec = -2), num = 10000)
sim(logitZFull, x = setx(logitZFull, polity2 = 10, timeUntilAnyElec = -1), x1 = setx(logitZFull, polity2 = 10, timeUntilAnyElec = 0), num = 10000)

# marginal effects (i.e., the marginal contribution of each variable on the scale of the linear predictor) 
# or 'partial effects' (i.e., the contribution of each variable on the outcome scale,
# conditional on the other variables involved in the link function transformation of the linear predictor)
interactPlotDemoME <- as.data.frame(cplot(logitFull, x = "polity2", dx = "timeUntilAnyElec", what = "effect"))
interactPlotDemoME$yvals <- as.numeric(interactPlotDemoME$yvals)

### left panel

pdf("../figures/fig2A.pdf")
ggplot(interactPlotDemoME,  aes(x=xvals, y=yvals)) + geom_line(aes(y=yvals)) +
  geom_line(aes(y=upper), color='black', linetype = 4, size=1.25) + 
  geom_line(aes(y=lower), color='black', linetype = 4, size=1.25) + 
  geom_hline(yintercept =0, linetype=2) +
  labs(x="Polity", y="Marginal effect of time until election\non Pr(violation=1)") + 
  theme_classic() + theme(legend.position = "none", legend.text=element_text(size=20),
                          legend.title=element_text(size=25),
                          axis.text=element_text(size=24), axis.title=element_text(size=28))

dev.off()

interactPlotDemoME1 <- as.data.frame(cplot(logitFull, x = "timeUntilAnyElec", dx = "polity2", what = "effect"))
interactPlotDemoME1$yvals <- as.numeric(interactPlotDemoME1$yvals)

### right panel

pdf("../figures/fig2B.pdf")
ggplot(interactPlotDemoME1,  aes(x=xvals, y=yvals)) + geom_line(aes(y=yvals)) +
  geom_line(aes(y=upper), color='black', linetype = 4, size=1.25) + 
  geom_line(aes(y=lower), color='black', linetype = 4, size=1.25) + 
  geom_hline(yintercept =0, linetype=2) +
  lims(x=c(-4,0.5), y=c(-0.00015, .0003))+ 
  labs(x="Time until election (years)", y="Marginal effect of level of democracy\non Pr(violation=1)") + 
  theme_classic() + theme(legend.position = "none", legend.text=element_text(size=20),
                          legend.title=element_text(size=25),
                          axis.text=element_text(size=23), axis.title=element_text(size=28))
dev.off()
