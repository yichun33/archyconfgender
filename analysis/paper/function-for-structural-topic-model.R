

stm_plot <- function(conf_paper_titles,
                     conf_gender_results,
                     max.em.its = 500,
                     k = 0,
                     n_words = 12,
                     first_name_appears_first = TRUE,
                     lower_thresh = 15){

  library(here)
  library(tidyverse)
  library(readxl)
  library(quanteda)
  library(stm)
  library(dplyr)

  x <- conf_paper_titles
  d <- conf_gender_results

  x$session <- as.character(x$session)
  d$session <- as.character(d$session)


if (first_name_appears_first) {

  # do nothing to the names

} else {

  # for CAA which has lastname, firstname (affiliation)
  # extract the first names
  x$FirstName <- map_chr(x$FirstName,
                                     ~tolower(str_split(str_split(.x, ".,")[[1]][2],
                                                        boundary("word"))[[1]][1]))
}

the_data <- left_join(d, x, by = c("name" = "FirstName",
                               "session" = "session"))

processed <-
  textProcessor(the_data$AbstractTitle,
                metadata = the_data)

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
                lower.thresh = lower_thresh)

set.seed(33)

the_model <<-
  stm(
    max.em.its = max.em.its,
    seed = 33,
    documents = out$documents,
    vocab = out$vocab,
    K = k,
    # watch the output to see how many topics it picks
    prevalence =  ~ gender,
    data = out$meta,
    init.type = "Spectral"
  )

# saveRDS(the_model, here::here("analysis/data/derived-data/the_model.rds"))
# the_model <- readr::read_rds(here::here("analysis/data/derived-data/the_model.rds"))

# if(exists("the_model")){
#   message("the model object exists")
# } else {
#   message("where is the model object?")
# }

# print(str(the_model))

topic_labels <-
  data.frame(labelTopics(the_model, n = n_words)$frex)

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

K_est <<- the_model$settings$dim$K

prep <- estimateEffect(1:K_est ~ gender,
                       the_model,
                       meta = out$meta,
                       uncertainty = "Global")

topic <-
  data.frame(
    topicnames = topic_labels_chr,
    TopicNumber = 1:the_model$settings$dim$K,
    TopicProportions = colMeans(the_model$theta),
    stringsAsFactors = F
  )

Result <-
plot(
  prep,
  method = "difference",
  covariate = "gender",
  cov.value1 = "female",
  cov.value2 = "male",
  model = the_model,
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
    model = the_model,
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

plot <-
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
                     xmin = ci_lower,
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
  xlab("    ← more men  -  more women →") +
  ylab("Topic")

# unlink(here::here("analysis/data/derived-data/the_model.rds"))


return(list(model = the_model,
            plot = plot))



}



stm_model_and_plot <-
  function(...){
    stm_output <- stm_plot(...)
    rm(the_model, K_est, envir = globalenv())
    return(stm_output)
  }


# ggsave(here::here("analysis/figures/stm-covariate-plot.png"),
#        height = 10, width = 6)






