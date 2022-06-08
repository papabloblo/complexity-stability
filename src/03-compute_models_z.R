library(glmnet)
library(tidyverse)
library(tictoc)

source("src/00-aux/getModel.R")
source("src/00-aux/data_generation.R")

path_in <- "data/02-clean_datasets/"
files <- list.files(path_in)

if (!dir.exists("data/03-z/")){
  dir.create("data/03-z/")
}

n_bootstrap <- 100

n_lambda <- 1000
lambda <- 10**seq(0, -6, length.out = n_lambda)

i <- 0
for (file in files){
  tic()
  i <- i + 1
  cat("\nTraining", file, "(", i, "of", length(files), ")\n")
  df <- read_csv(paste0(path_in, file), show_col_types = FALSE)
  
  target_name <- names(df)[1]
  
  if (mean(df[[target_name]]) < .1){
    cat("  Number of classes below 0.1% ")
  } else {
    n_var <- ncol(df) - 1
    
    n <- nrow(data)
    
    z <- expand_grid(lambda, bootstrap = 1:n_bootstrap)
    
    z <- bind_cols(z, matrix(0, 
                             ncol = n_var + 1, 
                             nrow = 1, 
                             dimnames =  list(
                               paste0('B', 1),
                               c(names(df)[2:ncol(df)], "loglik")
                               )
                             )
                   )
    
    predictors_names <- names(df)[names(df) != target_name]
    p <- 1    
    for (b in 1:n_bootstrap){
      
      if (b == 1 | b %% 10 == 0){
        tic()
        cat("  Bootstrap sample", b, " ")  
      }
      
      
      train_oob <- bootstrap_generation(df)
      
      train_x <- as.matrix(train_oob$train[, predictors_names])
      train_y <- factor(train_oob$train[[target_name]], levels = c(0, 1))
      
      oob_x <- as.matrix(train_oob$oob[, predictors_names])
      oob_y <- factor(train_oob$oob[[target_name]], levels = c(0, 1))
      
      model <- fit_lasso(train_x, train_y, oob_x, oob_y, lambda)
      
      
      z[z$bootstrap == b, 3:(ncol(df)-1+2)][1:nrow(model$z), ] <- as.matrix(model$z)
      z[z$bootstrap == b, 'loglik'][1:nrow(model$z),] <-  apply(model$pred, 
                                              2, 
                                              function(prob) loglikelihood(prob, train_oob$oob[[target_name]]))
      
      p <- p + 1
      if (p == 9){
        cat(toc()$msg)
        p <- 0
      }
      
    }
    
    
    write_csv(z, paste0("data/03-z/", file))
   
  }
  
  cat(toc()$msg)
}

  