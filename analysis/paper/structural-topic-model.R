library(here)
library(tidyverse)
library(readxl)
library(quanteda)
x <-
  readxl::read_excel(here("analysis/data/raw-data/2018 SAA Title.xlsx"))

library(stm)
library(dplyr)

d <- readRDS(here::here("analysis/data/derived-data/gender unnest"))

data <- left_join(d, x, by = c("name" = "FirstName",
                               "SessionNumber" = "session"))

processed <-
  textProcessor(data$AbstractTitle,
                metadata = data)

out <-
  prepDocuments(processed$documents,
                processed$vocab,
                processed$meta)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

out <-
  prepDocuments(processed$documents,
                processed$vocab,
                processed$meta,
                lower.thresh = 15)

set.seed(33)
model <-
  stm(
    documents = out$documents,
    vocab = out$vocab,
    K = 0,
    # watch the output to see how many topics it picks
    prevalence =  ~ gender,
    data = out$meta,
    init.type = "Spectral"
  )

topic_labels <-
  data.frame(labelTopics(model, n = 12)$frex)

topic_labels_chr <-
topic_labels %>%
  t %>%
  as_tibble() %>%
  map(function(...) {
    current <- tibble(...)
     current  %>%
       mutate_all(as.character) %>%
       unlist(., use.names=FALSE) %>%
       paste0(., collapse = ", ")
  }) %>%
  unlist %>%
  unname

out$meta$gender <- as.factor(out$meta$gender)

prep <- estimateEffect(1:model$settings$dim$K ~ gender,
                       model,
                       meta = out$meta,
                       uncertainty = "Global")

topic <-
  data.frame(
    topicnames = topic_labels_chr,
    TopicNumber = 1:model$settings$dim$K,
    TopicProportions = colMeans(model$theta),
    stringsAsFactors = F
  )

Result <-
plot(
  prep,
  method = "difference",
  covariate = "gender",
  cov.value1 = "female",
  cov.value2 = "male",
  model = model,
  omit.plot = TRUE
)


trank = order(unlist(Result$means))
temp.topic <- topic[trank, ]

Result <-
  plot(
    prep,
    method = "difference",
    covariate = "gender",
    cov.value1 = "female",
    cov.value2 = "male",
    model = model,
    topics = temp.topic$TopicNumber,
    labeltype = "custom",
    custom.labels = temp.topic$topicnames,
    cex = 0.01,
    omit.plot = TRUE
  )

Result_tbl <-
tibble(means = unlist(Result$means),
       topics = unlist(Result$topics),
       labels = unlist(Result$labels),
       ci_lower  = map_dbl(Result$cis, ~.x[1]),
       ci_upper = map_dbl(Result$cis, ~.x[2]),
       order = 1:length( unlist(Result$means))
)

highlight_tbl <-
  Result_tbl %>%
  filter(ci_lower > 0 | ci_upper < 0)

normal_tbl <-
  Result_tbl %>%
  filter(ci_lower < 0) %>%
  filter(ci_upper > 0)

highlight_size = 2.2
normal_size = 2.2
normal_colour = "grey30"

ggplot() +
  theme_bw() +
  geom_vline(xintercept = 0,
             colour = "red") +
  geom_linerange(data = normal_tbl,
               aes(y = order,
                   xmin =ci_lower,
                   xmax = ci_upper),
               colour = normal_colour) +
  geom_linerange(data = highlight_tbl,
                 aes(y = order,
                     xmin =ci_lower,
                     xmax = ci_upper),
                 colour = "red") +
  geom_linerange(data = highlight_tbl,
                 aes(y = order,
                     xmin =ci_lower,
                     xmax = ci_upper),
                 colour = "red") +
  geom_point(data = normal_tbl,
             aes(means,
                 order),
             colour = normal_colour) +
  geom_point(data = highlight_tbl,
             aes(means,
                 order),
             pch = 17,
             colour = "red",
             size = 2) +
  geom_text(data = normal_tbl,
            aes(means,
                order + 0.5,
                label = labels),
            size = normal_size) +
  geom_text(data = highlight_tbl,
            aes(means,
                order + 0.5,
                label = labels),
            size = highlight_size,
            colour = "red") +
  xlab("        ← more men  ...  more women →") +
  ylab("Topic")






