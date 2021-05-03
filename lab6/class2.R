library(tidyverse)
employed <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/employed.csv")

# make data palatable
employed_tidy <- employed %>%
  filter(!is.na(employ_n)) %>%
  group_by(occupation = paste(industry, minor_occupation), race_gender) %>%
  summarise(n = mean(employ_n)) %>%
  ungroup()

# further cleaning
employment_demo <- employed_tidy %>%
  filter(race_gender %in% c("Women", "Black or African American", "Asian")) %>%
  pivot_wider(names_from = race_gender, values_from = n, values_fill = 0) %>%
  janitor::clean_names() %>%
  left_join(employed_tidy %>%
              filter(race_gender == "TOTAL") %>%
              select(-race_gender) %>%
              rename(total = n)) %>%
  filter(total > 1e3) %>%
  mutate(across(c(asian, black_or_african_american, women), ~ . / (total)), 
         total = log(total),
         across(where(is.numeric), ~ as.numeric(scale(.))) # center and scale values for better kmeans output (later)
  ) %>%
  mutate(occupation = snakecase::to_snake_case(occupation))

employment_demo

# remove occupation column (will be hard to model with)
employment_clust <- kmeans(select(employment_demo, -occupation), centers = 3)
summary(employment_clust)

# take value of cluster and add it to the dataframe
library(broom)
tidy(employment_clust)

augment(employment_clust, employment_demo) %>%
  ggplot(aes(total, black_or_african_american, color = .cluster)) +
  geom_point()

# make the k clusters
kclusts <-
  tibble(k = 1:9) %>%
  mutate(
    kclust = map(k, ~ kmeans(select(employment_demo, -occupation), .x)),
    glanced = map(kclust, glance),
  )

# can see the clusters 
kclusts %>%
  unnest(cols = c(glanced)) %>%
  ggplot(aes(k, tot.withinss)) +
  geom_line(alpha = 0.5, size = 1.2, color = "midnightblue") +
  geom_point(size = 2, color = "midnightblue") 