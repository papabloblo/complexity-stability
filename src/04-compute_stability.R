library(tidyverse)

source("src/00-aux/stability.R")

path_in <- "data/03-z/"
files <- list.files(path_in)

files <- str_remove(files, ".csv")

list_df <- list()
for (f in files){
  z <- read_csv(paste0(path_in, f, ".csv")) 
  
  z <- na.omit(z)
  z <- z[is.finite(z$loglik),]
  
  lambda <- unique(z$lambda)
  
  sta <- map_dbl(lambda, function(x) stability(as.matrix(z[z$lambda == x, 3:(ncol(z)-1) ])))
  med_loglik <- map_dbl(lambda, function(x) mean(z$loglik[z$lambda == x], na.rm = TRUE))
  
  list_df[[f]] <- tibble(
    dataset = str_remove(f, "z-"),
    lambda = lambda, 
    loglik = med_loglik, 
    stability = sta
  )
  
}

df_loglik_stability <- bind_rows(list_df)

write_csv(df_loglik_stability, "data/04-loglik_stability.csv")