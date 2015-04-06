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
#Most of the columns are mostly NA's...

##Get rid of columns with more than 50%(TBA) NA's:
ind = apply(Xtrain, 2, function(x) {sum(is.na(x)) > 0.5*length(x)})
col_ind = c(1:ncol(Xtrain))[ind]
length(col_ind)
# 1159 such columns will be removed.
sum(col_ind <= 118)
# 93 out of 116 numeric are invalid.
sum(col_ind > 118 & col_ind <= 329)
# 194 out of 210 ordinal are invalid.
sum(col_ind > 329)
# 872 out of 1052 categorical are invalid.

#To combine what's left:
X = cbind(Ytrain[,-1], Xtrain[,-c(1,2,col_ind)])

######Algorithms:
ols = function(Xtrain, Ytrain, Xtest)
{
  betahat = solve(t(Xtrain)%*%Xtrain)%*%t(Xtrain)%*%Ytrain
  Yhat = Xtest %*% betahat
  return (Yhat)
}

forward_stepwise = function(Xtrain, Ytrain, Xtest, k)  
{
  included_predictors = 1  ## include the intercept by default
  for (i in 1:k)
  {
    potential_predictors = (1:ncol(Xtrain))[-included_predictors]
    RSS = rep(0,length(potential_predictors))
    for (j in 1:length(potential_predictors))
    {
      predictors = c(included_predictors, potential_predictors[j])
      trainYhat = ols(Xtrain[,predictors,drop=FALSE], Ytrain, Xtrain[,predictors,drop=FALSE])
      RSS[j] = sum((Ytrain - trainYhat)^2) 
    }
    best_new_predictor = potential_predictors[which(RSS == min(RSS))] ## Smallest RSS is same as biggest R^2
    included_predictors = c(included_predictors, best_new_predictor)
  }
  Yhat = ols(Xtrain[,included_predictors], Ytrain, Xtest[,included_predictors])
  return(Yhat)
}

pcr = function(Xtrain, Ytrain, Xtest, k)  
{
  V = svd(Xtrain)$v[, 1:k, drop=FALSE]
  Wtrain = Xtrain %*% V
  betahat = solve(t(Wtrain)%*%Wtrain) %*% t(Wtrain) %*% Ytrain
  Wtest = Xtest %*% V
  Yhat = Wtest%*%betahat
  return(Yhat)
}

ridgeregression = function(Xtrain, Ytrain, Xtest, lambda)
{
  p = ncol(Xtrain)
  betahat = solve(t(Xtrain)%*%Xtrain + lambda*diag(p))%*%t(Xtrain)%*%Ytrain
  Yhat = Xtest %*% betahat
  return (Yhat)
}