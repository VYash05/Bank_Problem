#Importing Libraries
library(tidyverse)
library(rockchalk)
library(caret)
#Importing Dataset
training_set <- read.csv('bank-full.csv', sep = ';')
test_set <- read.csv('bank.csv', sep = ';')
dataset <- rbind(training_set, test_set)

#Data Preprocessing
summary(dataset)

# Quick Data exploration (Missing values)
library(DataExplorer)
#Plot missing values
plot_missing(dataset)
#Continuous variables
plot_histogram(dataset)
#Categorical Variables
plot_bar(dataset)

#Encoding Categorical Data

#Combining levels with similar proportions to avoid redundancy
round(prop.table(table(dataset$job,dataset$y),1)*100,1)
dataset$job = combineLevels(dataset$job, levs = c("self-employed","unknown","technician"), newLabel = "job_1")
dataset$job = combineLevels(dataset$job, levs = c("services","housemaid","entrepreneur", "blue-collar"), 
                            newLabel = "job_2")
dataset$job = combineLevels(dataset$job, levs = c("management", "admin."), newLabel = "job_3")
dataset$job = combineLevels(dataset$job, levs = c("student"), newLabel = "job_4")
dataset$job = combineLevels(dataset$job, levs = c("retired"), newLabel = "job_5")
dataset$job = combineLevels(dataset$job, levs = c("unemployed"), newLabel = "job_6")

dataset$job =  factor(dataset$job,
                         levels = c("job_1", "job_2", "job_3", "job_4", "job_5", "job_6"),
                         labels = c(1, 2,3,4,5,6))

table(dataset$marital)
dataset$marital = factor(dataset$marital,
                         levels = c("divorced", "married", "single"),
                         labels = c(1, 2, 3))

table(dataset$education)
dataset$education = factor(dataset$education,
                           levels = c("primary", "secondary", "tertiary", "unknown"),
                           labels = c(1,2,3,4))

dataset$default = factor(dataset$default,
                         levels = c("no", "yes"),
                         labels = c(0,1))

dataset$housing = factor(dataset$housing,
                         levels = c("no", "yes"),
                         labels = c(0,1))

dataset$loan = factor(dataset$loan,
                      levels = c("no", "yes"),
                      labels = c(0,1))

table(dataset$contact)

dataset$contact = factor(dataset$contact,
                         levels = c("cellular", "telephone", "unknown"),
                         labels = c(1,2,3)) 

table(dataset$month)
#Combining levels with similar count to avoid redundancy
round(prop.table(table(dataset$month,dataset$y),1)*100,1)
dataset$month = combineLevels(dataset$month, levs = c("dec", "sep", "oct"), newLabel = "month1")
dataset$month = combineLevels(dataset$month, levs = c("aug","jan", "jun", "jul", "nov"), newLabel = "month2")
dataset$month = combineLevels(dataset$month, levs = c("mar"), newLabel = "month3")
dataset$month = combineLevels(dataset$month, levs = c("apr"), newLabel = "month4")
dataset$month = combineLevels(dataset$month, levs = c("feb"), newLabel = "month5")
dataset$month = combineLevels(dataset$month, levs = c("may"), newLabel = "month6")

dataset$month = factor(dataset$month,
                       levels = c("month1", "month2", "month3", "month4", "month5", "month6"),
                       labels = c(1,2,3,4,5,6))


dataset$poutcome = factor(dataset$poutcome,
                          levels = c("failure", "other", "success", "unknown"),
                          labels= c(1,2,3,4))

dataset$y = factor(dataset$y,
                   levels = c("no", "yes"),
                   labels = c(0,1))

# Duration is not counted to create a more realistic model
dataset = dataset[-12]

#Splitting dataset
library(caTools)
set.seed(123)
split = sample.split(dataset$y, SplitRatio = 0.80)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Fitting Random Forest Classification to the Training set
library(randomForest)
set.seed(123)
classifier = randomForest(x = training_set[-16],
                          y = training_set$y,
                          ntree = 100)

# Predicting the Test set results
y_pred = predict(classifier, newdata = test_set[-16])

confusionMatrix(y_pred, test_set$y)
varImp(classifier)


#Therefore, a supervised model has been created to predict term deposit product with an accuracy of 91%. 

