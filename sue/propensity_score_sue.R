# Load necessary libraries
library(MatchIt)
library(dplyr)

#load gss.RData
dat <- read.csv("data/clean_data.csv")

#Outcome-model Standardization
lmod <- glm(cancer ~ hrt + race + Hispanic + surgmeno, family = binomial,data = dat)

dat0 <- dat1 <- dat
dat0$hrt <- 0
dat1$hrt <- 1


EYhat0 <- predict(lmod, newdata = dat0, type = "response")
EYhat1 <- predict(lmod, newdata = dat1, type = "response")

EY0 <- mean(EYhat0)
EY0
EY1 <- mean(EYhat1)
EY1

#Exposure-model Standardization
e <- fitted(glm(hrt ~ race + Hispanic + surgmeno, family = binomial,data = dat))
dat$W <- (1 / e) * dat$hrt + (1 / (1 - e)) * (1 - dat$hrt)

beta <- glm(cancer ~ hrt, data = dat, weights = W)$coef

EY0 <- beta[1]
EY0
EY1 <- beta[1] + beta[2]
EY1

#Propensity-score Distributions
a <- range(density(e[dat$hrt == 1], bw = .05)$y)
b <- range(density(e[dat$hrt == 0], bw = .05)$y)
a <- range(a, b)
# Set up the plot
plot(c(0, 1),
     a,
     type = "n",
     xlab = "propensity score",
     ylab = "density")
# Add the lines
lines(density(e[dat$hrt == 1], bw = .05), lty = 1)
lines(density(e[dat$hrt == 0], bw = .05), lty = 2)
# Add a legend
legend('topleft', c("hrt=0", "hrt=1"), lty = c(2, 1),cex=0.5)

#Using the Propensity Score in the Outcome Model (Treatment Effect Estimates)
emod <-glm(hrt ~ race + Hispanic + surgmeno, family = binomial,data = dat)
e <- fitted(emod)
lmod <- glm(cancer ~ hrt + e, family = binomial, data = dat)
dat0 <- dat1 <- dat
dat0$hrt <- 0
dat1$hrt <- 1
dat0$e <- dat1$e <- e

EYhat0 <- predict(lmod, newdata = dat0, type = "response")
EYhat1 <- predict(lmod, newdata = dat1, type = "response")

EY0 <- mean(EYhat0)
EY0 
EY1 <- mean(EYhat1)
EY1

rd <- EY1 - EY0
rd

#Using the Propensity Score in the Outcome Model (Conditional Treatment Effect Estimates)
summary(glm(formula = cancer ~ hrt + e, family = binomial, data = dat))


#Stratification on the Propensity Score
out<-glm(hrt~race + Hispanic + surgmeno, family=binomial, data=dat)
eb=fitted(out)
quartiles <- quantile(eb, c(0,.5, .75, 1)) # 25 and 50 are same, causes error

quartiles

equartiles <-cut(eb, breaks = quartiles, include.lowest = T)

out <- glm(cancer ~ hrt * equartiles - 1 - hrt, data = dat)

EY0 <- out$coef[1:4]
EY1 <- out$coef[1:4] + out$coef[5:8]

RD <- mean(c(EY1 - EY0))
RD

for (i in 1:3){
  print(c(EY0[i],EY1[i]))
}



#Matching on the Propensity Score
if (!require("Matching")) install.packages("Matching")
library(Matching)

summary(Match(Y=dat$cancer,Tr=dat$hrt,X=e,estimand="ATE",caliper=.25,replace=T,ties=F))
sqrt(var(e))

summary(Match(Y=dat$cancer,Tr=dat$hrt,X=e,estimand="ATT",caliper=.25,replace=T,ties=F))
MatchBalance(cancer~race + Hispanic + surgmeno, data=dat, match.out=match.out)
