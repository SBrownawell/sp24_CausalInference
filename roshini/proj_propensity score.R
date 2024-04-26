# set directory
# Load necessary libraries
library(MatchIt)
library(dplyr)

# Read the dataset
data <- read.csv("data.csv")

# Define treatment, outcome, and covariates
treatment <- "hrt"
outcome <- "cancer"
covariates <- c("menopaus", "agegrp", "density", "race", "Hispanic", "bmi", "agefirst", "nrelbc", "brstproc", "lastmamm", "surgmeno")

# Filter data to include only treated and untreated individuals
data <- data[data$hrt %in% c(0, 1), ]

# Build propensity score model
ps_model <- glm(hrt ~ menopaus + agegrp + density + race + Hispanic + bmi + agefirst + nrelbc + brstproc + lastmamm + surgmeno, data = data, family = binomial())
data$propensity_score <- predict(ps_model, type = "response")

# Filter data based on specified conditions
data <- data %>%
  filter(menopaus == 1 ) %>%
  filter(density < 9 ) %>%
  filter(race < 9 ) %>%
  filter(Hispanic < 9 ) %>%
  filter(bmi < 9 ) %>%
  filter(agefirst < 9 ) %>%
  filter(nrelbc < 9 ) %>%
  filter(brstproc < 9 ) %>%
  filter(lastmamm < 9 ) %>%
  filter(surgmeno < 9 ) %>%
  filter(hrt < 9 )

matched <- matchit(hrt ~ menopaus + agegrp + density + race + Hispanic + bmi + agefirst + nrelbc + brstproc + lastmamm + surgmeno, method = "nearest", data = data)
matched_data <- match.data(matched)
summary(matched)
# Outcome-model Standardization
lmod <- glm(cancer ~ menopaus + agegrp + density + race + Hispanic + bmi + agefirst + nrelbc + brstproc + lastmamm + surgmeno, family = binomial, data = matched_data)
dat0 <- dat1 <- matched_data
dat0$hrt <- 0
dat1$hrt <- 1
EYhat0 <- predict(lmod, newdata = dat0, type = "response")
EYhat1 <- predict(lmod, newdata = dat1, type = "response")
EY0 <- mean(EYhat0)
EY0
EY1 <- mean(EYhat1)
EY1

# Exposure-model Standardization
e <- fitted(glm(hrt ~ menopaus + agegrp + density + race + Hispanic + bmi + agefirst + nrelbc + brstproc + lastmamm + surgmeno, family = binomial, data = matched_data))
dat0$hrt <- 0
dat1$hrt <- 1
EYhat0 <- predict(lmod, newdata = dat0, type = "response")
EYhat1 <- predict(lmod, newdata = dat1, type = "response")
EY0 <- mean(EYhat0)
EY1 <- mean(EYhat1)
EY0
EY1

# Propensity-score Distributions
a <- range(density(e[matched_data$hrt == 1], bw = .05)$y)
b <- range(density(e[matched_data$hrt == 0], bw = .05)$y)
a <- range(a, b)

# Set up the plot
plot(c(0, 1), a, type = "n", xlab = "propensity score", ylab = "density")
# Add the lines
lines(density(e[matched_data$hrt == 1], bw = .05), lty = 1)
lines(density(e[matched_data$hrt == 0], bw = .05), lty = 2)
# Add a legend
legend('topleft', c("hrt=0", "hrt=1"), lty = c(2, 1), cex = 0.5)

# Using the Propensity Score in the Outcome Model (Treatment Effect Estimates)
emod <- glm(cancer ~ menopaus + agegrp + density + race + Hispanic + bmi + agefirst + nrelbc + brstproc + lastmamm + surgmeno, family = binomial, data = matched_data)
e <- fitted(emod)
lmod <- glm(hrt ~ menopaus + agegrp + density + race + Hispanic + bmi + agefirst + nrelbc + brstproc + lastmamm + surgmeno + e, family = binomial, data = matched_data)
dat0 <- dat1 <- matched_data
dat0$hrt <- 0
dat1$hrt <- 1
dat0$e <- dat1$e <- e
EYhat0 <- predict(lmod, newdata = dat0, type = "response")
EYhat1 <- predict(lmod, newdata = dat1, type = "response")
EY0 <- mean(EYhat0)
EY1 <- mean(EYhat1)
treatment_effect <- EY1 - EY0

# Using the Propensity Score in the Outcome Model (Conditional Treatment Effect Estimates)
summary(glm(formula = cancer ~ hrt + e, family = binomial, data = matched_data))

# Stratification on the Propensity Score
out <- glm(hrt ~ menopaus + agegrp + density + race + Hispanic + bmi + agefirst + nrelbc + brstproc + lastmamm + surgmeno + factor(propensity_score), family = binomial, data = matched_data)
quartiles <- quantile(matched_data$propensity_score, c(0, .25, .5, .75, 1))
equartiles <- cut(matched_data$propensity_score, breaks = quantile(matched_data$propensity_score, c(0, .25, .5, .75, 1)), include.lowest = TRUE)
stratified_out <- glm(cancer ~ hrt * equartiles - 1 - hrt, data = matched_data)
EY0_strat <- coef(stratified_out)[1:4]
EY1_strat <- EY0_strat + coef(stratified_out)[5:8]
conditional_treatment_effect <- mean(EY1_strat - EY0_strat)

# Matching on the Propensity Score
if (!require("Matching")) install.packages("Matching")
library(Matching)
match_out <- Match(Y = matched_data$cancer, Tr = matched_data$hrt, X = matched_data$propensity_score, estimand = "ATT", caliper = 0.25, replace = TRUE)
summary(match_out)

# Standard error of the propensity score
sqrt(var(matched_data$propensity_score))