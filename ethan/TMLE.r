
# R-package tmle 
# Note: in 2018, the base super learner library was SL.step, SL.glm and SL.glm.interaction
# this changed and thus we explicitely specify the library here; still the results are not exactly identical to those reported in the paper


# importing packages
if(!require(tmle)){
	install.packages("tmle", repos="https://cloud.r-project.org/")
	library(tmle)
}

if(!require(SuperLearner)){
	install.packages("SuperLearner", repos="https://cloud.r-project.org/")
	library(SuperLearner)
}

if(!require(randomForest)){
	install.packages("randomForest", repos="https://cloud.r-project.org/")
}

# checking loaded data
if(!exists("loadedData"))
#loadedData <- read.csv("D:/Desktop/DS HW/sp24_CausalINference/ethan/clean_data.csv")

#data_file <- paste(getwd(), "clean_data.csv", sep="/")
data_file <- paste(getwd(), "../data/clean_data.csv")
print(data_file)
loadedData <- read.csv(data_file)


# R-package tmle with user-selected Super learner libraries
# note: you need the following packagesto be installed (not necessarily loaded): randomForest, gam, rpart
# as in Box 9, there are minor changes in the results due to updates of packages

SL.library <- c("SL.glm","SL.step","SL.step.interaction", "SL.glm.interaction","SL.gam",
                "SL.randomForest", "SL.rpart") 

TMLE3 <- tmle(Y = loadedData$cancer,A = loadedData$hrt,W = loadedData [,c("Hispanic", "menopaus", "race", "surgmeno")
], 
              family = "binomial", Q.SL.library = SL.library,g.SL.library = SL.library)#, gbound=0)



ATEtmle3 <- TMLE3$estimates$ATE$psi;ATEtmle3
TMLE3$estimates$ATE$CI
MORtmle3 <- TMLE3$estimates$OR$psi;MORtmle3
TMLE3$estimates$OR$CI

