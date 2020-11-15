#install.packages('rpart')
library(rpart)
#install.packages("rpart.plot")
library(rpart.plot)
#setwd("C:/Users/wei_chen/Desktop/")
data = read.csv("campaign.csv", sep=";")
head(data)
summary(data)

treeModel = rpart(y ~ . , data = data)
rpart.plot(treeModel)
treeModel
probability = predict(treeModel, newdata = data)
prediction = probability[,2]>0.5
actual = data$y
table(actual, prediction)

idxTrain = sample(4521, 3000)
train = data[idxTrain, ]
validation = data[-idxTrain, ]
default.model = rpart(y ~ ., data = train)
info.model = rpart(y~., data=train, parms=list(split="information"))

probability = predict(default.model, newdata=validation)
prediction = probability[,2]>0.5
actual = validation$y
result = table(actual, prediction)
(result[1,1]+result[2,2])/sum(result)

overfit.model = rpart(y~., data=train, minsplit=2, 
                      minbucket=1, cp=0.0001)
probability = predict(overfit.model, newdata=validation)
prediction = probability[,2]>0.5
actual = validation$y
result = table(actual, prediction)
(result[1,1]+result[2,2])/sum(result)

