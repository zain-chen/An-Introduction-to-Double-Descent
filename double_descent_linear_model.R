## load the needed package
library(MASS)

## set the simulation parameters
beta = runif(1000) # real coefficients
beta = beta/sqrt(sum(beta^2)) # convert to a unit vector
M = 3 # number of simulations
N = c(seq(200, 800, 50), seq(900, 990, 10), seq(991,1000,1), 
      seq(1001, 1009, 1), seq(1010, 1100, 10), seq(1200, 10000, 400)) # number of samples
test_MSE = matrix(nrow = length(N), ncol = M)

## conduct the simulation
for (i in 1:length(N)){
  for (m in 1:M){
    # generate training data
    X = replicate(1000, rnorm(N[i]))
    e = rnorm(N[i], sd = 0.1)
    y = X %*% beta + e
    
    # estimate the beta_hat
    if (N[i] < 1000){
      beta_hat = ginv(X) %*% y # pseudo inverse 参照论文用了加号逆
    } else {
      dat = as.data.frame(cbind(y, X))
      names(dat)[1] = "y"
      lm_model = lm(y ~ .-1, data = dat) # regular fit of linear model
      beta_hat = matrix(lm_model$coefficients, ncol = 1)
    }
    
    # generate test set
    X_test = replicate(1000, rnorm(10000))
    e_test = rnorm(10000, sd = 0.1)
    y_test = X_test %*% beta + e_test
    
    # measure model accuracy
    preds_test = X_test %*% beta_hat
    test_MSE[i, m] = sqrt(mean((y_test - preds_test)^2))
  }
}

## plot the data
plot(1000/N,apply(test_MSE, 1, mean), ylab = "Test MSE", xlab = "p / n", 
     pch = 4,
     ylim = c(0,20))
lines(1000/N, apply(test_MSE, 1, mean), col = "red", lwd = 2)
