

# functions for joining table

 x <-readRDS(here::here("analysis/data/derived-data/SAA18_topic_model.rds"))
 y <-readRDS(here::here("analysis/data/derived-data/SAA18_gender_model.rds"))

joining_table_topic_and_gender <- function(x, y)

library(dplyr)
library(tidyverse)
joined_table <-
  left_join(x$unique_id_gamma,
            y,
            by = c("FirstName" = "name",
                   "session" = "session"))


#Making table similar to Stats Rethinking



joined_table %>%
  filter(!is.na(gender)) %>%
  filter(gamma >= 0.1) %>%
ggplot(aes(x = as.factor(topic), y = gamma)) +
  geom_boxplot() +
  facet_wrap(~gender, ncol = 1)


joined_table %>%
  filter(!is.na(gender)) %>%
  filter(gamma >= 0.1) %>%
  mutate(new_gamma = ((gamma - mean(gamma)) / sd(gamma))^(1/2)) %>%
  ggplot(aes(x = as.factor(topic),
             y = new_gamma,
             colour = gender)) +
  geom_boxplot()


joined_table %>%
  filter(!is.na(gender)) %>%
  filter(gamma >= 0.1) %>%
  mutate(new_gamma = ((gamma - mean(gamma)) / sd(gamma))^(1/2)) %>%
  filter(!is.nan(new_gamma)) %>%
  mutate_at(vars(topic, gender),
            as.factor) %>%
  aov(gamma ~ gender * topic, data = . ) %>%
  summary

d <-
  joined_table %>%
  filter(!is.na(gender)) %>%
 # filter(gamma >= 0.1) %>%
  mutate(new_gamma = ((gamma - mean(gamma)) / sd(gamma))^(1/2)) %>%
  filter(!is.nan(new_gamma)) %>%
  mutate(topic = paste0('topic', topic)) %>%
  select(-gamma) %>%
  pivot_wider(names_from = topic,
              values_from = new_gamma,
              values_fill = list(new_gamma = 0))

