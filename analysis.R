# Set working directory
setwd("~/Documents/GitHub/coursera-datascience-practical-machine-learning-project")

# Load relevant libraries
library(caret)

# Load in training and test datasets
train <- read.csv("pml-training.csv")
test <- read.csv("pml-testing.csv")

# Create training and testing datasets
inTrain <- createDataPartition(y = )

# Need to clean dataset