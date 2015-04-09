#Import data
Xtrain <- read.csv("C:/Users/yuan/src/CountableCare/Data/TrainValue.csv")
Ytrain <- read.csv("C:/Users/yuan/src/CountableCare/Data/TrainLabel.csv")
Xtest <- read.csv("/C:/Users/yuan/src/CountableCare/Data/TestValue.csv")

#Introduce NA's for white spaces in categorical data:
Xtrain[Xtrain==""] <- NA

#Take a loot into NA's
vec=rep(0, ncol(Xtrain))
for (i in 1:ncol(Xtrain)){
  vec[i] = sum(is.na(Xtrain[,i]))
}
plot(1:1379, vec, type="h",  col="red")
abline(v=118, col="blue", lwd=3)
abline(v=329, col="blue",lwd=3)

hist(vec)
sum(is.na(Xtrain))/(ncol(Xtrain)*nrow(Xtrain))
#Most of the columns are mostly NA's...

##Get rid of columns with more than 83%(TBA) NA's:
ind = apply(Xtrain, 2, function(x) {sum(is.na(x)) > 0.83*length(x)})
col_ind = c(1:ncol(Xtrain))[ind]
length(col_ind)
# 1004 such columns will be removed.
sum(col_ind <= 118)
# 93 out of 116 numeric are invalid.
sum(col_ind > 118 & col_ind <= 329)
# 194 out of 210 ordinal are invalid.
sum(col_ind > 329)
# 872 out of 1052 categorical are invalid.

#To combine what's left:
X = cbind(Ytrain[,-1], Xtrain[,-c(1,col_ind)])

#Deal with numeric missing values:
num = grep("n_", colnames(X))
for (i in 1:nrow(X)) {
X[i, num] <- ifelse(is.na(X[i, num]), 0, X[i, num])}

#Deal with ordinal missing values:
ord = grep("o_", colnames(X))
for (i in 1:nrow(X)) {
  X[i, ord] <- ifelse(is.na(X[i, ord]), -1, X[i, ord])}

#Deal with categorical missing values:
X = as.matrix(X)
cat = grep("c_", colnames(X))
for (i in 1:nrow(X)) {
  X[i, cat] <- ifelse(is.na(X[i, cat]), "Missing", X[i, cat])}

X = as.data.frame(X)

#Make dummies for categorical, and release:
#train = X[,c(1:14, 16:55)]
#dum_release = model.matrix(~X[,15])
#train = cbind(train, dum_release[,-1])
#for (i in 56:233){
#  dummy = model.matrix(~X[,i])
#  train = cbind(train, dummy[,-1])
#}

#Change to numeric matrix:
#train = as.matrix(train)
#class(train) <- "numeric"

#### Now for test data:
Xtest[Xtest==""] <- NA
test = Xtest[,-c(1,col_ind)]
#Deal with numeric missing values:
num = grep("n_", colnames(test))
for (i in 1:nrow(test)) {
  test[i, num] <- ifelse(is.na(test[i, num]), 0, test[i, num])}
#Deal with ordinal missing values:
ord = grep("o_", colnames(test))
for (i in 1:nrow(test)) {
  test[i, ord] <- ifelse(is.na(test[i, ord]), -1, test[i, ord])}
#Deal with categorical missing values:
test = as.matrix(test)
cat = grep("c_", colnames(test))
for (i in 1:nrow(test)) {
  test[i, cat] <- ifelse(is.na(test[i, cat]), "Missing", test[i, cat])}

Xtrain = X[,-c(1:14)]
Ytrain = X[,1:14]
Xtest = test
data = list(Xtrain, Ytrain, Xtest, 0.83)
names(data) = c("Xtrain", "Ytrain", "Xtest", "na_cutoff")
save(data, file="data.Rda")

#Make dummies for categorical, and release:
#test = as.data.frame(test)
#x_test = test[,c(2:41)]
#dum_release = model.matrix(~test[,1])
#x_test = cbind(x_test, dum_release[,-1])
#for (i in 42:219){
#  dummy = model.matrix(~test[,i])
#  x_test = cbind(x_test, dummy[,-1])
#}
#Change to numeric matrix:
#x_test = as.matrix(x_test)
#class(x_test) <- "numeric"

###Submission format:
write_submission <- function(probs, model_name) {
  file_path <- file.path("submit", paste0(model_name, ".csv"))
  submit <- read.csv("Data/SubmissionFormat.csv")
  submit[, 2:ncol(submit)] <- probs
  if (file.exists(file_path))
    stop(paste0(file_path, " already exists!"))
  write.csv(submit, file.path(file_path), row.names = FALSE)
  message(paste0("Results written to ", file_path))
}

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

##Model fitting
library(glmnet)

cv_mod_ridge <- cv.glmnet(Xtrain, as.matrix(Ytrain[,1]), alpha = 0)
# plot(cv_mod_ridge)
# pred_ridge <- predict(cv_mod_ridge, s = cv_mod_ridge$lambda.min, newx = as.matrix(test_data))
# write_submission(ytest = pred_ridge, model_name = "ridge1")