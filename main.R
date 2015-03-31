#Import data
Xtrain <- read.csv("C:/Users/yuan/src/CountableCare/Data/TrainValue.csv")
Ytrain <- read.csv("C:/Users/yuan/src/CountableCare/Data/TrainLabel.csv")
Xtest <- read.csv("C:/Users/yuan/src/CountableCare/Data/TestValue.csv")

#Algorithms:
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

#play
Xtest = as.matrix(Xtest)
Xtrain = as.matrix(Xtrain)
Ytrain = as.matrix(Ytrain)
Y_ols = ols(Xtrain, Ytrain, Xtest)
