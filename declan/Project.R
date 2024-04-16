library(tidyverse)
library(tidymodels)
library(readr)

risk <- read.table("risk.txt",header=TRUE)

labels = c("meno", "agegrp", "density", "race", "Hispanic", "bmi", "agefirst", 
           "nrelbc", "brstproc", "lastmamm", "surgmeno", "hrt", "invasive", 
           "cancer", "training", "count")

for (i in 1:16) {
  names(risk)[i] <- labels[i]
}

#View(risk)

mylogit <- glm(cancer ~ meno + agegrp + density + race + Hispanic + bmi + 
                 agefirst + nrelbc + brstproc + lastmamm + surgmeno + hrt + 
                 invasive, data = risk, family = binomial)
summary(mylogit)
confint(mylogit)

########### Creating a predictive model #################
# Split data into train and test
risk$cancer = as.factor(risk$cancer)

set.seed(429)
split <- initial_split(risk, prop = 0.8, strata = cancer)
train <- split %>% 
  training()
test <- split %>% 
  testing()

# FIGURING OUT HYPERPARAMETERS

# # Define the logistic regression model with penalty and mixture hyperparameters
# log_reg <- logistic_reg(mixture = tune(), penalty = tune(), engine = "glmnet")
# 
# # Define the grid search for the hyperparameters
# grid <- grid_regular(mixture(), penalty(), levels = c(mixture = 4, penalty = 3))
# 
# # Define the workflow for the model
# log_reg_wf <- workflow() %>%
#   add_model(log_reg) %>%
#   add_formula(cancer ~ .)
# 
# # Define the resampling method for the grid search
# folds <- vfold_cv(train, v = 5)
# 
# # Tune the hyperparameters using the grid search
# log_reg_tuned <- tune_grid(
#   log_reg_wf,
#   resamples = folds,
#   grid = grid,
#   control = control_grid(save_pred = TRUE)
# )
# 
# select_best(log_reg_tuned, metric = "roc_auc")

# BEST VALUES WERE 1 FOR MIXTURE AND 0.0000000001 FOR PENALTY

model <- logistic_reg(mixture=1, penalty=0.0000000001) %>%
  set_engine("glmnet") %>%
  set_mode("classification") %>%
  fit(cancer ~ ., data = train)

tidy(model)

# Class Predictions
pred_class <- predict(model,
                      new_data = test,
                      type = "class")

# Class Probabilities
pred_proba <- predict(model,
                      new_data = test,
                      type = "prob")
results <- test %>%
  select(cancer) %>%
  bind_cols(pred_class, pred_proba)

accuracy(results, truth = cancer, estimate = .pred_class)

