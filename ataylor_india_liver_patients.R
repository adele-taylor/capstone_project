### R script file for all analysis conducted per report.Rmd
### Created: AT 08 JAN 2021

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
library(knitr)
library(kableExtra)
library(tidyverse)
library(caret)
library(data.table)

#download file from https://archive.ics.uci.edu/ml/machine-learning-databases/00225/

URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00225/Indian%20Liver%20Patient%20Dataset%20(ILPD).csv"
download.file(URL, destfile="data.csv")
raw_data <- read.table("temp.csv", 
                  header=F, stringsAsFactors=T, sep=",", row.names=NULL)
colnames(raw_data) <- c("Age", "Gender", "Total Bilirubin", "Direct Bilirubin", "Alkaline Phosphotase", "Alamine Aminotransferase", "Aspartate Aminotransferase", "Total Protien", "Albumin", "Albumin/Globulin Ratio", "Disease")
raw_data <- raw_data %>% mutate(Disease=(Disease-1))

#remove any rows is NA
cleaned_data <- na.omit(raw_data)

# Validation set will be 10% of full dataset
set.seed(18, sample.kind="Rounding") 
test_index <- createDataPartition(y = cleaned_data$Age, times = 1, p = 0.1, list = FALSE)
training_set <- cleaned_data[-test_index,]
validation_set <- cleaned_data[test_index,]

models <- c("glm", "knn", "rf", "rpart", "lda", "qda")

RMSE <- function(real, predicted){
sqrt(mean((real-predicted)^2))
}

#initially train on subset to check whether models are at all feasible

n <- nrow(training_set)
m <- n/10

set.seed(4, sample.kind="Rounding")
test_models <- sapply(models,function(model){
  test_subset <- training_set[sample(n,m),]
  train_subset <- anti_join(training_set, test_subset)
  fit <- train(Disease ~ ., model=model, data=train_subset)
  return(RMSE(predict(fit, test_subset),test_subset$Disease))
})

test_models %>% kable() %>% kable_styling()

#build ensemble
final_model <- function(validation_set, training_set){
  predictions <- sapply(models, function(model){
      fit <- train(Disease ~ ., model=model, data=training_set)
      return((predict(fit, validation_set)))
  })      
      }

#run final validation

final_predictions <- final_model(validation_set, training_set)

final_rmse <- RMSE(final_predictions, validation_set$Disease)
