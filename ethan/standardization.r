



if(!require(CausalModels)) {
	install.packages("CausalModels", repos="https://cloud.r-project.org/")
	library(CausalModels)
}


data_file_loc <- paste(getwd(), "../data/clean_data.csv", sep="/")
print(data_file_loc)
loadedData <- read.csv(data_file_loc)

confounders <- c("Hispanic", "surgmeno", "race")

loadedData$hrt <- as.factor(loadedData$hrt)

init_params(cancer, hrt, confounders, loadedData)

model <- standardization(data=loadedData, family=binomial, simple=TRUE, n.boot=50)

ip_model <- ipweighting(data=loadedData, f=cancer~hrt+Hispanic+surgmeno+race, family=binomial, p.simple=TRUE, n.boot=50)

summary(model)
summary(ip_model)
