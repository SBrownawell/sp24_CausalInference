# Load necessary libraries
if (!require("MatchIt")) install.packages("MatchIt")
library(MatchIt)

if (!require("dplyr")) install.packages("dplyr")
library(dplyr)

dat <- read.csv("../data/clean_data.csv")

dat$race = factor(dat$race)

#Outcome-model Standardization
print("Outcome-model Standardization")
lmod <- glm(cancer ~ hrt + race + Hispanic + surgmeno, family = binomial,data = dat)

dat0 <- dat1 <- dat
dat0$hrt <- 0
dat1$hrt <- 1


EYhat0 <- predict(lmod, newdata = dat0, type = "response")
EYhat1 <- predict(lmod, newdata = dat1, type = "response")

EY0 <- mean(EYhat0)
EY1 <- mean(EYhat1)

RD <- EY1 - EY0
print("EY0")
print(EY0)
print("EY1")
print(EY1)
print("RD")
print(RD)

#Exposure-model Standardization
print("Exposure-model Standarization")
e <- fitted(glm(hrt ~ race + Hispanic + surgmeno, family = binomial,data = dat))
dat$W <- (1 / e) * dat$hrt + (1 / (1 - e)) * (1 - dat$hrt)

beta <- glm(cancer ~ hrt, data = dat, weights = W)$coef

EY0 <- beta[1]
EY1 <- beta[1] + beta[2]
print("EY0")
print(EY0)
print("EY1")
print(EY1)

RD <- EY1 - EY0
print("RD")
print(RD)

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
print("Propensity score in Outcome Model")
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
EY1 <- mean(EYhat1)

rd <- EY1 - EY0

print("EY0")
print(EY0)
print("EY1")
print(EY1)
print("RD")
print(rd)

#Using the Propensity Score in the Outcome Model (Conditional Treatment Effect Estimates)
print("Propensity Score in Outcome Model (Conditional Treatment Effect Estimates")
print(summary(glm(formula = cancer ~ hrt + e, family = binomial, data = dat)))

#Stratification on the Propensity Score
print("Stratification on the Propensity Score")
out<-glm(hrt~race + Hispanic + surgmeno, family=binomial, data=dat)
print(summary(out))
eb=fitted(out)

barplot(table(eb))

#quartiles <- quantile(eb, c(0,.5, 1)) # 25 and 50 are same, causes error
quads <- quantile(eb, c(0, 0.3, 0.9, 1)) # 25 and 50 are same, causes error

for(i in quads) {
	print(i)
}

equads <-cut(eb, breaks = quads, include.lowest = T)

out <- glm(cancer ~ hrt * equads - 1 - hrt, data = dat)

print("summary(out)")
print(summary(out))
print("out$coef")
print(out$coef)
EY0 <- out$coef[1:length(quads)-1]
EY1 <- out$coef[1:length(quads)-1] + out$coef[length(quads):(2*(length(quads)-1))]

print("EY0")
print(EY0)
print("EY1")
print(EY1)

print("EY1-EY0")
print(EY1-EY0)

RD <- mean(c(EY1 - EY0))

print("RD")
print(RD)

if(TRUE) {
	#Matching on the Propensity Score
	if (!require("Matching")) install.packages("Matching")
	library(Matching)

	print(summary(Match(Y=dat$cancer,Tr=dat$hrt,X=e,estimand="ATE",caliper=.25,replace=T,ties=F)))
	sqrt(var(e))

	print(summary(Match(Y=dat$cancer,Tr=dat$hrt,X=e,estimand="ATT",caliper=.25,replace=T,ties=F)))
	MatchBalance(cancer~race + Hispanic + surgmeno, data=dat, match.out=match.out)
}
