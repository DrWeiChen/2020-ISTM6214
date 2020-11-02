library(class)
# load data
uBank = read.csv('UniversalBank.csv', header = T)

head(uBank)
attach(uBank)
length(which(Personal.Loan==1))/length(Personal.Loan)

set.seed(123457)
N = length(Personal.Loan)
train.id = sort(sample(N, N*0.6))
validate.id = seq(N)[-train.id]

train_input = scale(uBank[train.id, c(2,3,4,6,7,8,9,11,12,13,14)])
validate_input = scale(uBank[validate.id, c(2,3,4,6,7,8,9,11,12,13,14)])
train_output = uBank[train.id, c(10)]
validate_output = uBank[validate.id, c(10)]

prediction_train = knn(train_input, train_input, train_output, k=3)
prediction_validate = knn(train_input, validate_input, train_output, k=3)

error_train = mean(abs(as.numeric(as.character(prediction_train))-train_output))
error_validate = mean(abs(as.numeric(as.character(prediction_validate))-validate_output))

# Error Rate Plot
error_train_list = seq(20)*0
error_validate_list = seq(20)*0
for(i in seq(20)){
  prediction_train = knn(train_input, train_input, train_output, k=i)
  prediction_validate = knn(train_input, validate_input, train_output, k=i)
  error_train_list[i] = mean(abs(as.numeric(as.character(prediction_train))-train_output))
  error_validate_list[i] = mean(abs(as.numeric(as.character(prediction_validate))-validate_output))
}

plot(seq(20), error_train_list, col='blue', type='l', xlab='k', ylab='Error rate')
lines(seq(20), error_validate_list, col='red')  


# Lift Charts
liftChart = function(i){
  prediction_validate_p = knn(train_input, validate_input, train_output, k=i, prob=T)
  idx.desc = order(prediction_validate_p, decreasing=T)
  lift.value = cumsum(validate_output[idx.desc])
  plot(seq(length(prediction_validate_p)), lift.value, type='l', 
       xlab='Number of Cases', ylab='Cumulative Success')
  lines(c(0,2000),c(0,sum(validate_output)),col='red')
}
liftChart(3)
liftChart(4)
