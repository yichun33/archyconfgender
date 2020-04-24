library(here)
library(tidyverse)
library(readxl)
library(quanteda)
x <-
  readxl::read_excel(here("analysis/data/raw-data/2018 SAA Title.xlsx"))

myCorpus <- corpus(x$AbstractTitle)
dfm <-
  dfm(
    myCorpus,
    remove = c(stopwords("english")),
    stem = F,
    remove_numbers = T,
    remove_punct = T,
    remove_symbols = T,
    split_hyphens = F
  )

topfeatures(dfm, 25)

dfm <- dfm_trim(dfm, min_docfreq = 2)

topfeatures(dfm, 25)

library(RColorBrewer)
textplot_wordcloud(
  dfm,
  scale = c(3.5, 0.75),
  colors = brewer.pal(8, "Dark2"),
  random.order = F,
  rot.per = 0.1,
  max.words = 100
)


stmdfm <- convert(dfm, to = "stm", docvars = docvars(myCorpus))





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

model <-
  stm(
    documents = out$documents,
    vocab = out$vocab,
    K = 0,
    # watch the output to see how many topics it picks
    prevalence =  ~ gender,
    data = out$meta,
    max.em.its = 10,
    init.type = "Spectral"
  )

labelTopics(model)

topic_labels <-
  data.frame(labelTopics(model)$prob)

out$meta$gender <- as.factor(out$meta$gender)

prep <- estimateEffect(1:20 ~ gender,
                       model,
                       meta = out$meta,
                       uncertainty = "Global")

topic <-
  data.frame(
    topicnames = c(
      "Problem Solving",
      "Time Management",
      "Provides Support",
      "Ask Questions",
      "Shows Respect"
    ),
    TopicNumber = 1:k,
    TopicProportions = colMeans(stmFit$theta),
    stringsAsFactors = F
  )

Result <-
plot(
  prep,
  method = "difference",
  covariate = "gender",
  cov.value1 = "female",
  cov.value2 = "male",
  model = model
)



trank = order(unlist(Result$means))
temp.topic <- topic[trank, ]
