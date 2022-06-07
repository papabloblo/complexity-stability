library(tidyverse)

loglik_stability <- read_csv("data/04-loglik_stability.csv")


files <- list.files("data/04-complexity_measures/", full.names = TRUE)
df <- map_df(files, function(x)readRDS(x))

df$dataset <- list.files("data/04-complexity_measures/")
df$dataset <- str_remove(df$dataset, ".rds")


max_stability <- loglik_stability %>% 
  group_by(dataset) %>% 
  filter(stability == max(stability, na.rm = TRUE)) %>% 
  filter(loglik == max(loglik, na.rm = TRUE)) %>% 
  rename(lambda_max_stability = lambda,
         loglik_max_stability = loglik,
         stability_max_stability = stability)


max_loglik <- loglik_stability %>% 
  group_by(dataset) %>% 
  filter(loglik == max(loglik, na.rm = TRUE)) %>%
  filter(is.finite(stability)) %>% 
  filter(stability == max(stability, na.rm = TRUE )) %>% 
  rename(lambda_max_loglik = lambda,
         loglik_max_loglik = loglik,
         stability_max_loglik = stability)


loglik_stability <- max_stability %>% 
  full_join(max_loglik, by = "dataset") %>% 
  full_join(df, by = "dataset")


write_csv(loglik_stability, "data/05-stability_complexity.csv")
