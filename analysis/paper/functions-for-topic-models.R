

# function to compute topic model for a conf

# x is the data frame from the spreadsheet

do_the_topic_model <- function(x, k) {

  d <- x

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
    unite(unique_id, session, FirstName) %>%
    count(unique_id, word, sort = TRUE) %>%
    ungroup()

  unique_id_dtm <- word_counts %>%
    cast_dtm(unique_id, word, n)

  library(topicmodels)
  unique_id_lda <- LDA(unique_id_dtm,
                       k = k,
                       control = list(seed = 1234))

  top_terms <-  tidy(unique_id_lda,
                     matrix = "beta")  %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

  unique_id_gamma <- tidy(unique_id_lda, matrix = "gamma")

  unique_id_gamma <- unique_id_gamma %>%
    separate(document, c("session", "FirstName"),
             sep = "_",
             convert = TRUE)

  return(list(unique_id_dtm = unique_id_dtm,
              unique_id_lda = unique_id_lda,
              top_terms = top_terms,
              unique_id_gamma = unique_id_gamma))

}



# access the different elements with $

# compute the optimum number of topics

compute_optimum_n_topics <- function(dtm){
  ldatuning::FindTopicsNumber(
    dtm,
   topics = seq(from = 2, to = 500, by = 1),
   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
   method = "Gibbs",
   control = list(seed = 77),
   mc.cores = 2L,
   verbose = TRUE
 )
}

# CAA18_tm_n_topics <- compute_optimum_n_topics(CAA18_tm$unique_id_dtm)
# ldatuning::FindTopicsNumber_plot(CAA18_tm_n_topics)

