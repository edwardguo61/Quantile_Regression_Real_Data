# load packages
library(plyr)
library(quantreg)

globalEstimate <- function(X, Y, Tau){
  # Return global estimates & standard error of estimates
  
  # Args
  # X;Y - ROE data
  # Tau - quantile of regression
  
  # fit a quantile regression
  rq_global <- rq(Y~X, tau = Tau)
  
  # skip the intercept
  beta_global <- coef(rq_global)
  se_global <- summary(rq_global,se="iid")$coefficients[, 2]
  
  return(list(beta = beta_global, se = se_global))
}

oneshotEstimate <- function(X, Y, Tau, index = "data$Year"){
  # Return one-hot estimate
  
  # Args
  # X;Y - ROE data
  # Tau - quantile of regression
  # index - variable used as group indicator
  
  P <- dim(X)[2]
  
  # combine X, Y and index together
  DData <- as.data.frame(cbind(Y, X, index = eval(parse(text=index))))
  colnames(DData)[1] <- 'Y'
  
  returnBetaSE <- function(x){
    rq_temp <- rq(Y~.-index, x, tau = Tau)
    return(c(coef(rq_temp), summary(rq_temp,se="iid")$coefficients[, 2]))
  }
  
  # fit a quantile regression for each machine
  result <- ddply(DData, .(index), returnBetaSE)
  
  # return mean of coefficients for all machines
  # return se of coefficients for all machines
  return(list(beta = apply(result[,2:(P+2)], 2, mean), 
              se = apply(result[,(P+3):(2*P+3)], 2, mean)))
}

onestepEstimate <- function(x, Y, Tau){
  # Return one step estimate
  
  # Args
  # x;Y - ROE data
  # Tau - quantile of regression
  
  # Define Gaussian Kernel Function
  G_kernel <- function(u){
    return((2*pi)^(-1/2) * exp(-u^2/2))
  }
  
  X <- as.matrix(x)
  
  # Acquire pilot sample
  N <- dim(X)[1]                 # number of observations
  P <- dim(X)[2]                 # number of dependent varaibles
  n <- round(sqrt(N) * log(N))   # number of pilot sample
  index <- sample(1:N, n)        # select pilot sample
  X_pilot <- as.matrix(X[index,])# get X for pilot sample
  Y_pilot <- Y[index]            # get Y for pilot sample
  
  # Pilot estimation
  # fit quantile regression on pilot sample
  rq_pilot <- rq(Y_pilot~X_pilot, tau = Tau)
  # skip the intercept
  beta_pilot <- coef(rq_pilot)[-1]
  beta_pilot_intercept <- coef(rq_pilot)[1]
  
  # error of pilot sample
  error_in_Q <- Y_pilot - X_pilot %*% beta_pilot - beta_pilot_intercept
  
  # send pilot estimators to branches to get pilot error
  error <- Y - X %*% beta_pilot - beta_pilot_intercept
  
  # estimate kernel density at 0
  IQR <- quantile(error_in_Q)[4] - quantile(error_in_Q)[2] # interquantile range
  A <- min(sd(error_in_Q), IQR/1.34)                       # get constant A
  h <- 0.9 * A * n^(-1/5)                                  # calculate bandwidth
  f0 <- sum(G_kernel(error/h)) / (N*h)                     # kernel density
  
  # update pilot estimation to get the one-step estimation
  X <- cbind(1, X)
  X_square_inverse <- solve(t(X) %*% X)
  beta_one_step <- c(beta_pilot_intercept, beta_pilot) + 
    (1/f0) * X_square_inverse %*% (t(X) %*% (Tau - 1*(error<=0)))
  
  # Calculate standard error of estimates
  omega_square <- Tau * (1-Tau) * f0^(-2)
  var_onestep <- omega_square * X_square_inverse
  se_onestep <- sqrt(diag(var_onestep))
  
  return(list(beta = as.vector(beta_one_step), se = se_onestep))
}

rqLoss <- function(X, Y, tau, beta){
  # Return loss of quantile regression
  
  # Args
  # X;Y - ROE data
  # Tau - quantile of regression
  # beta - estimates of quantile regression
  
  # Define loss function
  rho <- function(u, tau)
  {
    return(u * (tau - 1 * (u <= 0)))
  }
  
  X <- cbind(1,X)
  return(sum(rho(Y - as.matrix(X) %*% beta, tau)))
}