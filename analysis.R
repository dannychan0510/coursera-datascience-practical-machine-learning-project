# Set working directory
setwd("~/Documents/GitHub/coursera-datascience-practical-machine-learning-project")

# Load relevant libraries
library(caret)
library(rattle)

# Load in training and test datasets
train <- read.csv("pml-training.csv", na.strings = c("NA", "", "#DIV/0!"))
test <- read.csv("pml-testing.csv", na.strings = c("NA", "", "#DIV/0!"))

# Checking dimension of training and testing data
dim(train)
dim(test)

# Delete columns with all missing values
train <- train[ , colSums(is.na(train)) == 0]
test <- test[ , colSums(is.na(test)) == 0]

# Drop variables that are not useful to the machine learning
train <- train[ , -c(1:7)]
test <- test[ , -c(1:7)]

# Segmenting training dataset for cross validation purposes using a 60:40 split
ss <- createDataPartition(y=train$classe, p=0.75, list=FALSE)
subTraining <- train[ss, ] 
subTesting <- train[-ss, ]

# Predicting with trees
set.seed(100)
modFitTree <- train(classe ~ ., method = "rpart", data = subTraining)

# Looking at the treeqpl predicted
print(modFitTree$finalModel)

# Plotting the plot tree
fancyRpartPlot(modFitTree$finalModel)

# Generating predictions
prediction_modFitTree <- predict(modFitTree, newdata = subTesting)
confusionMatrix(prediction_modFitTree, subTesting$classe)

# Predicting with Logistic Regressions
modFitLogit <- train(classe ~ ., method = "LogitBoost", data = subTraining)
prediction_modFitLogit <- predict(modFitLogit, newdata = subTesting)
confusionMatrix(prediction_modFitLogit, subTesting$classe)

# Predicting with Random Forests
modFitRF <- train(classe ~ ., method = "rf", data = subTraining)
prediction_modFitRF <- predict(modFitRF, newdata = subTesting)
confusionMatrix(prediction_modFitRF, subTesting$classe)