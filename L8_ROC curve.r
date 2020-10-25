# load beer example
#setwd('/Users/Your Folder Path')
beer <- read.csv('Beer+Preferences.txt', header = T)
beer$Income <- as.numeric(sub("\\$", "", beer$Income))
beer

Light <- ifelse(beer$Preference=="Light", 1, 0)
beer$Light <- Light
str(beer)


model.logit <- glm(Light~Gender+Married+Income+Age, data=beer, family = binomial)
summary(model.logit)
glm.probs = predict(model.logit, data = beer, type = "response")

glm.pred=rep("Regular", 100)
glm.pred[glm.probs >.5]="Light"
table(beer$Preference, glm.pred)

install.packages('ROCR')
library(ROCR)
pred <- prediction(glm.probs, beer$Light)
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize=T)


auc <- performance(pred, "auc")
auc <- unlist(slot(auc, "y.values"))
auc