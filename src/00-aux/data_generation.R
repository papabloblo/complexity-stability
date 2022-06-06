library(mvtnorm)

# TEST

# n_features <- 10
# n_instances <- 10
# n_irrelevant_features <- 5
# rho_redundancy <- .5
# mean_by_class <- c(1, -1)
# class_balance <- .25


mvnormal_dataset <- function(n_features,
                             n_instances,
                             n_irrelevant_features,
                             rho_redundancy = 0.5,
                             mean_by_class = c(1,-1),
                             class_balance = 0.5) {
  
  n_relevant_features <- n_features - n_irrelevant_features
  
  # Mu generation
  mu_class1 <- rep(0, n_features)
  mu_class1[1:n_relevant_features] <- mean_by_class[1]
  
  mu_class2 <- rep(0, n_features)
  mu_class2[1:n_relevant_features] <- mean_by_class[2]
  
  # Sigma generation
  sigma <- diag(n_features)
  
  sigma_aux <- diag(n_relevant_features)
  
  sigma_aux[lower.tri(sigma_aux)] <- rho_redundancy
  sigma_aux[upper.tri(sigma_aux)] <- rho_redundancy
  
  sigma[1:n_relevant_features, 1:n_relevant_features] <- sigma_aux
  
  # Matrix empty
  dt_matrix <- matrix(nrow = n_instances, ncol = n_features + 1)
  
  colnames(dt_matrix) <- c(paste0('x', 1:n_features), "y")
  
  # Data generation
  n_class1 <- ceiling(n_instances * class_balance)
  n_class2 <- n_instances - n_class1
  

  dt_matrix[1:n_class1, 1:n_features] <- rmvnorm(n = n_class1,
                                                 mean = mu_class1,
                                                 sigma = sigma
                                                 )
  
  dt_matrix[(n_class1 + 1):n_instances, 1:n_features] <- rmvnorm(
    n = n_class2,
    mean = mu_class2,
    sigma = sigma
    )
  
  
  dt_matrix[,"y"] <- c(
    rep(mean_by_class[1], n_class1),
    rep(mean_by_class[2], n_class2)
  )
  
  
  return(dt_matrix)
}



bootstrap_generation <- function(dt_matrix){
  
  n <- nrow(dt_matrix)
  
  s <- sample(1:n, size = n, replace = TRUE)
  
  return(
    list(
      train = dt_matrix[s,],
      oob = dt_matrix[-s,]
    )
  )
  
}

# TEST
# dt_matrix <- mvnormal_dataset(
#   n_features = 10,
#   n_instances = 30,
#   n_irrelevant_features = 5
# )
# 
# bootstrap_generation(dt_matrix)

