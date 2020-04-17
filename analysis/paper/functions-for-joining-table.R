

# functions for joining table

# x is a _tm file, y is dataframe

joining_table_topic_and_gender <- function(x, y)

library(dplyr)
library(tidyverse)
joined_table <-
  left_join(x$unique_id_gamma,
            x$genders_unnest,
            by = c("FirstName" = "name",
                   "SessionNumber" = "SessionNumber",
                   "LastName" = "LastName"))
saveRDS(joined_table, "final table")

#Making table similar to Stats Rethinking
joined_table_wider <-
joined_table %>%
  pivot_wider(names_from = "topic",
              values_from = "gamma")

old_names <- names(joined_table_wider)[5:25]
new_names <- paste0("topic", old_names)
renamed_joined_table <-
joined_table_wider %>%
  rename_at(vars(old_names), ~new_names) %>%
  mutate(gender_b = ifelse(gender == "female", 1, 0))


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
joined_d <- left_join(d, readRDS("gamma"))

m5.16 <- map(
  alist(
    gender ~ dbinom( mu , sigma ) ,
    mu <- a +
      b.t1*topic1 +
      b.t1*topic1 +
      b.t2*topic2 +
      b.t3*topic3 +
      b.t4*topic4 +
      b.t5*topic5 +
      b.t6*topic6 +
      b.t7*topic7 +
      b.t8*topic8 +
      b.t9*topic9 +
      b.t10*topic10 +
      b.t11*topic11 +
      b.t12*topic12 +
      b.t13*topic13 +
      b.t14*topic14 +
      b.t15*topic15 +
      b.t16*topic16 +
      b.t17*topic17 +
      b.t18*topic18 +
      b.t19*topic19 +
      b.t20*topic20 +
      b.t21*topic21 +
      b.g*gender,
    a ~ dnorm( 0.6 , 10 ) ,
    b.t ~ dnorm( 0 , 1 ) ,
    b.g ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 10 )
  ),
  data=joined_table )
precis(m5.16)

# just a few people
joined_table %>%
  filter(LastName == "Moss",
         SessionNumber == 130) %>%
  ggplot( aes(x = as.factor(topic),
             y = gamma)) +
  geom_boxplot()





joined_table %>%
  filter(topic == 4) %>%
  filter(gamma >= 0.1) %>%
  filter(!is.na(gender)) %>%
  mutate_at(vars(topic, gender),
            as.factor) %>%
  t.test(gamma ~ gender, data=.)


  ggplot( aes(x = as.factor(topic),
              y = gamma,
              colour = gender)) +
  geom_boxplot()


fit <- glm(gender_b ~ topic1 + topic2 + topic3 + topic4 + topic5 + topic6 +
             topic7 + topic8 + topic9 + topic10 + topic11 + topic12 + topic13 +
             topic14 + topic15 + topic16 +topic17 + topic18 +topic19 +
             topic20 +topic21, data = renamed_joined_table, family = binomial())
summary(fit)
