#install.packages("caret", dependencies = TRUE)
#install.packages("randomForest")

library(caret)
library(randomForest)

trainData <- read.csv('titanic_train.csv', sep=",", header= TRUE)
testData <- read.csv('titanic_test.csv', sep = ",", header = TRUE)

head(trainData)
head(testData)

# Converting 'Survived' to a factor
trainData$Survived <- factor(trainData$Survived)
# Set a random seed
set.seed(51)
# Training using 'random forest' algorithm
model <- train(Survived ~ Pclass + Sex + SibSp + 
Embarked + Parch + Fare, # Variables we decided to include
data = trainData, # Use the train data frame as the training data
method = 'rf',# Use the 'random forest' algorithm
trControl = trainControl(method = 'cv', # Use cross-validation
number = 5)) # Use 5 folds for cross-validation
               
model

# Summary of the Test Data
summary(testData)
testData$Fare <- ifelse(is.na(testData$Fare), mean(testData$Fare, na.rm = TRUE), testData$Fare)
testData$Survived <- predict(model, newdata = testData)
testData$Survived


#####################################################
# From Lecture Slides week 12
# Bagging and Random Forests
library(randomForest)
library(MASS)
data("Boston")
train = sample(506, 300)

# We first do bagging (which is just RF with m = p)
bag.boston=randomForest(medv~.,data=Boston,mtry=13,importance=TRUE,subset = train)
bag.boston

yhat.bag = predict(bag.boston,newdata=Boston[-train,])
boston.test = Boston[-train,]$medv
plot(yhat.bag, boston.test)
abline(0,1)

mean((yhat.bag-boston.test)^2)

importance(bag.boston)

# Random Forest
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
