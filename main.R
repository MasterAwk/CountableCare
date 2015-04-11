
## Source the algorithms:
source("algorithms.R")


###############DUMMY#############
makeDummy = function(train, test){
#Make dummies for categorical, and release:
dum_release = model.matrix(~train[,1])
train = cbind(dum_release[,-1], train[,-1])
first_cat = grep("c_", colnames(train))[1]
train1 = train[,c(1:(first_cat-1), (ncol(train)-2):ncol(train))]

dum_release = model.matrix(~test[,1])
test = cbind(dum_release[,-1], test[,-1])
test1 = test[,c(1:(first_cat-1), (ncol(test)-2):ncol(test))]

for (i in first_cat:(ncol(train)-3)){
  if (length(levels(train[,i])) >= length(levels(test[,i]))){
    dummy = model.matrix(~train[,i])
    train1 = cbind(train1, dummy[,-1])
  }
  else{
    extra = length(levels(test[,i]))-length(levels(train[,i]))
    mat0 = matrix(rep(0,nrow(train)*extra), ncol=extra)
    dummy = model.matrix(~train[,i])
    train1 = cbind(train1, dummy[,-1], mat0)
  }
}

for (i in first_cat:(ncol(train)-3)){
  if (length(levels(test[,i])) >= length(levels(train[,i]))){
    dummy = model.matrix(~test[,i])
    test1 = cbind(test1, dummy[,-1])
  }
  else{
    extra = length(levels(train[,i]))-length(levels(test[,i]))
    mat0 = matrix(rep(0,nrow(test)*extra), ncol=extra)
    dummy = model.matrix(~test[,i])
    test1 = cbind(test1, dummy[,-1], mat0)
  }
}

return(list(train1, test1))
}

load("C:/Users/yuan/src/jr_kaggle/data/data.rda")
train = data$train
test = data$test

dummy = makeDummy(train,test)
data_dummy_0.5 = list(dummy[[1]], dummy[[2]], data$ytrain, 0.5)
names(data_dummy_0.5) = c("train", "test", "ytrain", "na_cutoff")
save(data_dummy_0.5, file="data_dummy_0.5.Rdata")

load("C:/Users/yuan/src/jr_kaggle/data/data_cutoff0.8.rda")
train = data$train
test = data$test

dummy = makeDummy(train,test)
data_dummy_0.8 = list(dummy[[1]], dummy[[2]], data$ytrain, 0.8)
names(data_dummy_0.8) = c("train", "test", "ytrain", "na_cutoff")
save(data_dummy_0.8, file="data_dummy_0.8.Rdata")

##Model fitting (ridge with 50%)
train = data_dummy_0.5$train
test = data_dummy_0.5$test
ytrain = data_dummy_0.5$ytrain
train = as.matrix(train)
class(train) = "numeric"
test = as.matrix(test)
class(test) = "numeric"

ytrain = as.matrix(ytrain)
ytrain[ytrain=="yes"] <- 1
ytrain[ytrain=="no"] <- 0
class(ytrain) = "numeric"

# ridge regression
library(glmnet)

nCores <- parallel::detectCores()
doParallel::registerDoParallel(nCores)

lambdas.min = rep(0,14)
result = matrix(rep(0,nrow(test)*ncol(ytrain)), ncol=ncol(ytrain))

for (i in 1:14){
print(i)
cv_mod_ridge <- cv.glmnet(train, ytrain[,i], lambda=c(0.05,seq(0.1,0.5,0.1)), 
                          alpha = 0, parallel=TRUE)
lambdas.min[i] = cv_mod_ridge$lambda.min
pred_ridge <- predict(cv_mod_ridge, s = cv_mod_ridge$lambda.min, newx = test)
result[,i] <- pred_ridge
}

result1 = result
plot(x=1:14, y=lambdas.min, xlab="Column# in Y", ylab="Lambda.min", type="h",
     main="Lambdas with Smallest MSE Using Ridge_CV")

result[result<0] <- 0
result[result>1] <- 1
submit <- read.csv("Data/SubmissionFormat.csv")
submit[, 2:ncol(submit)] <- result
write.csv(submit, "ridge0.5.csv", row.names = FALSE)


##Model fitting (ridge with 80%):
train = data_dummy_0.8$train
test = data_dummy_0.8$test
ytrain = data_dummy_0.8$ytrain
train = as.matrix(train)
class(train) = "numeric"
test = as.matrix(test)
class(test) = "numeric"

ytrain = as.matrix(ytrain)
ytrain[ytrain=="yes"] <- 1
ytrain[ytrain=="no"] <- 0
class(ytrain) = "numeric"

# ridge regression
library(glmnet)

nCores <- parallel::detectCores()
doParallel::registerDoParallel(nCores)

lambdas.min = rep(0,14)
result = matrix(rep(0,nrow(test)*ncol(ytrain)), ncol=ncol(ytrain))

for (i in 1:14){
  print(i)
  cv_mod_ridge <- cv.glmnet(train, ytrain[,i], alpha = 0, parallel=TRUE)
  lambdas.min[i] = cv_mod_ridge$lambda.min
  pred_ridge <- predict(cv_mod_ridge, s = cv_mod_ridge$lambda.min, newx = test)
  result[,i] <- pred_ridge
}

result1 = result
plot(x=1:14, y=lambdas.min, xlab="Column# in Y", ylab="Lambda.min", type="h",
     lwd=3, main="Lambdas with Smallest MSE Using Ridge_CV")

result[result<0] <- 0.1
result[result>1] <- 0.9

submit <- read.csv("Data/SubmissionFormat.csv")
submit[, 2:ncol(submit)] <- result
write.csv(submit, "ridge0.8.csv", row.names = FALSE)
#Score: 0.6074

##Modify overbound values
ridge <- read.csv("C:/Users/yuan/src/CountableCare/ridge0.5.csv")
colmean = colMeans(ytrain)
for (i in 2:15){
  ridge[which(ridge[,i]==0),i] <- colmean[i-1]
  ridge[which(ridge[,i]==1),i] <- colmean[i-1]
}

write.csv(ridge, "ridge0.5_colmean.csv", row.names = FALSE)
#Score: 0.5044

ridge <- read.csv("C:/Users/yuan/src/CountableCare/ridge0.8.csv")
colmean = colMeans(ytrain)
for (i in 2:15){
  ridge[which(ridge[,i]==0),i] <- colmean[i-1]
  ridge[which(ridge[,i]==1),i] <- colmean[i-1]
}

write.csv(ridge, "ridge0.8_colmean.csv", row.names = FALSE)
#Score: 0.5274