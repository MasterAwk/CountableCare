ols = function(Xtrain, Ytrain, Xtest)
{
  betahat = solve(t(Xtrain)%*%Xtrain)%*%t(Xtrain)%*%Ytrain
  Yhat = Xtest %*% betahat
  return (Yhat)
}

gls = function(Xtrain, Ytrain, Xtest, G)
{
  betahat = solve(t(Xtrain)%*%solve(G)%*%Xtrain)%*%t(Xtrain)%*%solve(G)%*%Ytrain
  Yhat = Xtest %*% betahat
  return (Yhat)
}

fgls = function(Xtrain, Ytrain, Xtest)
{
  residuals = Ytrain - ols(Xtrain, Ytrain, Xtrain)
  G = diag(residuals^2)
  return (gls(Xtrain, Ytrain, Xtrain, G))
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

backward_stepwise = function(Xtrain, Ytrain, Xtest, k)  
{
  p = ncol(Xtrain)
  included_predictors = 1:p
  for (i in 1:(p-1-k))  ## one of the p columns is the intercept, and we want to leave that column in, along with k "real" columns.  So we remove p-1-k columns.
  {
    RSS = rep(0,length(included_predictors))
    for (j in 1:length(included_predictors))
    {
      predictors = included_predictors[-j]
      trainYhat = ols(Xtrain[,predictors,drop=FALSE], Ytrain, Xtrain[,predictors,drop=FALSE])
      RSS[j] = sum((Ytrain - trainYhat)^2) 
    }
    RSS[1] = 0  # protect the intercept from removal
    included_predictors = included_predictors[-which(RSS == max(RSS))] ## Largest RSS is same as smallest R^2
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

knn = function(Xtrain, Ytrain, Xtest, k) 
{
  Ntrain = nrow(Xtrain)
  Ntest = nrow(Xtest)
  Yhat = rep(0, Ntest)
  for (i in 1:Ntest)
  {
    distances = rep(0, Ntrain)
    for (j in 1:Ntrain) distances[j] = sqrt(sum((Xtest[i,] - Xtrain[j,])^2))
    knearest = order(distances)[1:k]
    Yhat[i] =  mean(Ytrain[knearest])
  }
  return(Yhat)
}







