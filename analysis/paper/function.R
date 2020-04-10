

# function to compute topic model for a conf

do_the_topic_model <- function(x) {

  d <- readxl::read_excel(here::here(x))

  library(tidyverse)
  library(tidytext)

  d_word <- d %>%
    unnest_tokens(word, AbstractTitle)

  word_counts <- d_word %>%
    anti_join(stop_words) %>%
    filter(!word %in% c("de", "la", "el", "los", "user",
                        "archaeology", "archaeological"
                        ,"site", "archaic", "sites")) %>%
    filter(!str_detect(word, "\\d")) %>%
    filter(str_length(word) >= 3) %>%
    filter(!str_detect(word, "\\.|'|\\<U")) %>%
    unite(unique_id, SessionNumber, LastName, FirstName) %>%
    count(unique_id, word, sort = TRUE) %>%
    ungroup()

  unique_id_dtm <- word_counts %>%
    cast_dtm(unique_id, word, n)

  library(topicmodels)
  unique_id_lda <- LDA(unique_id_dtm, k = 21, control = list(seed = 1234))

  top_terms <- unique_id_lda %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

  unique_id_gamma <- tidy(unique_id_lda, matrix = "gamma")

  unique_id_gamma <- unique_id_gamma %>%
    separate(document, c("SessionNumber", "LastName", "FirstName"), sep = "_", convert = TRUE)
}

do_the_topic_model("")
