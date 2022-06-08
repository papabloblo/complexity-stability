library(tidyverse)
library(corrr)

if (!dir.exists("results/")){
  dir.create("results")
}

stability_complexity <- read_csv("data/05-stability_complexity.csv")

perc_nas <- sapply(stability_complexity, function(x) mean(is.na(x)))
stability_complexity <- stability_complexity[, perc_nas < 1]

# Correlation

cor_df <- stability_complexity %>% 
  select(-dataset, -starts_with("lambda")) %>% 
  correlate(diagonal = 1) %>% 
  select(term, starts_with("loglik"), starts_with("stability")) %>% 
  filter(!(term %in% c("loglik_max_stability",
                     "stability_max_stability",
                     "loglik_max_loglik", 
                     "stability_max_loglik")
           )
         )

cor_df %>% 
  arrange(-abs(loglik_max_loglik))

cor_df %>% 
  arrange(-abs(stability_max_loglik))



stability_complexity2 <- stability_complexity %>% 
  select(-starts_with("lambda")) %>% 
  pivot_longer(overlapping.F1.mean:network.Hubs.sd, 
               names_to = "term",
               values_to = "value"
               )

stability_complexity2 <- stability_complexity2 %>% 
  left_join(stability_complexity_clus, by = "dataset")

p1 <- stability_complexity2 %>% 
  ggplot(aes(x = value, y = loglik_max_loglik
             #, color = as.character(clusterh)
             )) + 
  geom_point() +
  facet_wrap(~term, scales = "free") +
  labs(
    title = "Loglik"
  ) +
  theme_minimal()

ggsave("results/loglik_vs_complexity.png", p1, width = 30, height = 20, units = "cm")
ggsave("results/loglik_vs_complexity.pdf", p1, width = 30, height = 20, units = "cm")

p2 <- stability_complexity2 %>% 
  ggplot(aes(x = value, y = stability_max_loglik
             #, color = as.character(cluster)
             )
             ) + 
  geom_point() +
  facet_wrap(~term, scales = "free") +
  labs(
    title = "Stability"
  ) +
  theme_minimal()

ggsave("results/stability_vs_complexity.png", p2, width = 30, height = 20, units = "cm")
ggsave("results/stability_vs_complexity.pdf", p2, width = 30, height = 20, units = "cm")

stability_complexity %>% 
  ggplot(aes(x = stability_max_loglik, y = loglik_max_loglik)) +
  geom_point()

loglik_stability %>% 
  ggplot(aes(x = neighborhood.N4.mean, y = stability_max_loglik)) +
  geom_point()


loglik_stability %>% 
  ungroup() %>% 
  select(stability_max_stability, ends_with(".mean")) %>% 
  cor(use = "complete.obs")

stability_complexity_clus <- stability_complexity %>% 
  select(dataset, overlapping.F1.mean:network.Hubs.sd) %>% 
  na.omit()
stability_complexity_clus$cluster <- kmeans(select(stability_complexity_clus, -dataset), centers = 6)$cluster

dist_mat <- dist(select(stability_complexity_clus, -dataset), method = 'euclidean')

hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
stability_complexity_clus$clusterh <- cutree(hclust_avg, k = 3)
