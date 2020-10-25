# (1) read data.
credit = read.csv("credit.csv")

# (2) create a variable PROFITABLE
credit$PROFITABLE = as.factor(credit$NPV>0)
summary(credit)

# (3) create categorical variables.
credit$CHK_ACCT = as.factor(credit$CHK_ACCT)
credit$SAV_ACCT = as.factor(credit$SAV_ACCT)
credit$NUM_CREDITS = as.factor(credit$NUM_CREDITS)
credit$HISTORY = as.factor(credit$HISTORY)
credit$PRESENT_RESIDENT = as.factor(credit$PRESENT_RESIDENT)
credit$EMPLOYMENT = as.factor(credit$EMPLOYMENT)
credit$JOB = as.factor(credit$JOB)
credit$NUM_DEPENDENTS = as.factor(credit$NUM_DEPENDENTS)
credit$INSTALL_RATE = as.factor(credit$INSTALL_RATE)
credit$TYPE = as.factor(credit$TYPE)

summary(credit)

# (4) Partition the data using a seed value of 12345.
set.seed(12345)
inTrain = sort(sample(1:1000, 700, replace=F))
train = data.frame(credit[inTrain,]) 
validation = data.frame(credit[-inTrain,])

# (5) Estimate the logistic regression model
fit1 = glm(PROFITABLE~.-OBS-NPV, data=train, family="binomial")
summary(fit1)

# odds of education 0.37 (marginally significant)
exp(-6.676e-01)
# customer applying for a credit for educational purpose resulting in an account being profitable are 0.37 times the odds
# of a customer applying for a credit for "other" purpose, all other characteristics of the application being the same. 

# (6) sensitivity and specificity (require the validation confusion matrix)
cutoff = 0.5
actual = validation$PROFITABLE
predicted.probability = predict(fit1, newdata=validation, type = "response") 
predicted = predicted.probability > cutoff
error_matrix = table(actual, predicted)
error_matrix
# sensitivity
error_matrix[2,2]/sum(error_matrix[2,])
## [1] 0.8957346
# specifity
error_matrix[1,1]/sum(error_matrix[1,])
## [1] 0.494382

# (7) accuracy on the training data
train.actual = train$PROFITABLE
train.probability = predict(fit1, newdata=train, type = "response") 
train.predicted = train.probability > cutoff
train.em = table(train.actual, train.predicted)
train.em

train.accuracy = sum(train.em[1,1]+train.em[2,2])/sum(train.em)
# 0.7857143

# (8) accuracy on the validation data
accuracy = sum(error_matrix[1,1]+error_matrix[2,2])/sum(error_matrix)
# 0.7766667

# cost table
# (9) Opportunity Cost of Missing Positive Revenue
opportunityCost = mean(train$NPV[which(train$PROFITABLE=="TRUE")])
# (10) Cost of Customer Default
defaultCost = mean(train$NPV[which(train$PROFITABLE=="FALSE")])
# (11) Total Cost when cutoff is 0.5
defaultCost*error_matrix[1,2]-opportunityCost*error_matrix[2,1]

# cutoff 0.1
cutoff = 0.1
actual = validation$PROFITABLE
predicted.probability = predict(fit1, newdata=validation, type = "response") 
predicted = predicted.probability > cutoff
cutoff01_em = table(actual, predicted)
defaultCost*cutoff01_em[1,2]-opportunityCost*cutoff01_em[2,1]

# cutoff 0.9
cutoff = 0.9
actual = validation$PROFITABLE
predicted.probability = predict(fit1, newdata=validation, type = "response") 
predicted = predicted.probability > cutoff
cutoff09_em = table(actual, predicted)
defaultCost*cutoff09_em[1,2]-opportunityCost*cutoff09_em[2,1]

# Compare Revenue
opportunityCost*error_matrix[2,2]+defaultCost*error_matrix[1,2]-opportunityCost*error_matrix[2,1]
opportunityCost*cutoff01_em[2,2]+defaultCost*cutoff01_em[1,2]-opportunityCost*cutoff01_em[2,1]
opportunityCost*cutoff09_em[2,2]+defaultCost*cutoff09_em[1,2]-opportunityCost*cutoff09_em[2,1]

