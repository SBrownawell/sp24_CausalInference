library(tidyverse)
library(tidymodels)
library(readr)
library(dplyr)
library(car)
library(boot)
library(CausalModels)

bc <- read.csv("./Ethan/clean_data.csv",header=TRUE)

View(bc)

mylogit <- glm(cancer ~ hrt + race + Hispanic + surgmeno, data = bc, family = binomial)
summary(mylogit)
confint(mylogit)

rand_sampl <- bc[sample(nrow(bc), 10000), ]

my_function <- function(formula, data, ids) {
  dat<-data[ids,]
  my_glm<-glm(formula, family=binomial,data=dat)
  return(my_glm$coef)
}

boot.out <- boot(data=bc, statistic=my_function, R=2500, formula=cancer ~ hrt + race + Hispanic + surgmeno)

est<-summary(boot.out)$original
SE<-summary(boot.out)$bootSE
lci<-est-1.96*SE
uci<-est+1.96*SE
ci_labels <- c("Intercept", "hrt", "race", "Hispanic", "surgmeno")
list(ci_labels=ci_labels,est=est,lci=lci,uci=uci)
summary(boot.out)
