
library(tidyverse)
library(glmnet)


fit_lasso <- function(train_x, train_y,
                      test_x, test_y,
                      lambdas){
  
  fit <- glmnet(x = train_x, y = train_y, 
                family = "binomial", alpha = 1, 
                lambda = lambdas
                )
  
  z <- coef(fit)[-1,] # Suppressing intercept
  z[z != 0] <- 1
  
  z <- t(z)

  pred <- predict(fit, newx = test_x, type = "response")
  
  return(list(pred = pred, z = z))
  
}

loglikelihood <- function(prob, y){
  y[y == -1] <- 0
  return(mean(y*log(prob) + (1 - y)*log(1 - prob)))
} 