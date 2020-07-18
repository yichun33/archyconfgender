# computer gender by first name

# read in data

get_the_gender_data <- function(x, first_name_appears_first = TRUE){

  d <- x

library(tidyverse)
library(readxl)

total_sessions_in_conf <- max(d$session) # Look in the PDF

# clean up exotic symbols
# from http://stackoverflow.com/a/17517674/1036500
fromto <- read.table(text="
                     from to
                     Å¡ s
                     Å oe
                     Å¾ z
                     Ã ss
                     Ã¾ y
                     Ã th
                     Ã  a
                     Ã¡ a
                     Ã¢ a
                     Ã£ a
                     Ã¤ a
                     Ã¥ a
                     Ã¦ ae
                     Ã§ c
                     Ã¨ e
                     Ã© e
                     Ãª e
                     Ã« e
                     Ã E
                     Ã¬ i
                     Ã� i
                     Ã® i
                     Ã¯ i
                     Ã° d
                     Ã± n
                     Ã² o
                     Ã³ o
                     Ã´ o
                     Ãµ o
                     Ã¶ o
                     Ã¸ oe
                     Ã¹ u
                     Ãº u
                     Ã» u
                     Ã¼ u
                     Ã½ y
                     Ã¿ y
                     Ä g
                     ",header=TRUE)
# Then the function:

replace_foreign_chars <- function(dat,fromto) {
  for(i in 1:nrow(fromto) ) {
    dat <- gsub(fromto$from[i],fromto$to[i],dat)
  }
  dat
}

# tidy up accents and foreign characters
d$`first-named speaker` <- replace_foreign_chars(d$FirstName, fromto)

# extract first name
require("Hmisc")
library(purrr)
first_word_safe <- safely(first.word)

if(first_name_appears_first){

d$first_name <-
  map(d$`first-named speaker`, first_word_safe) %>%
  map(., c(1)) %>%
  flatten_chr()

} else {

  # for CAA which has lastname, firstname (affiliation)
  d$`first-named speaker` <- map_chr(d$`first-named speaker`,
    ~tolower(str_split(str_split(.x, ".,")[[1]][2],
                      boundary("word"))[[1]][1]))

}

# fill down on session numbers because I only numbered the first speaker in each session.
library(tidyr)
d <- fill(d, session)

# remove dupicate names in each session
d <-
  d %>%
  group_by(session) %>%
  distinct(`first-named speaker`, .keep_all = TRUE )


# compute gender of first name
library(gender)
library(genderdata)
library(dplyr)

# make a safe function so we get a result even if it's an error
gender_safe <-  safely(gender)

# identify the gender of each first nane, this may take a few moments...
d_genders <-
  d  %>%
  # hold out name col to use it to make list-col
  nest_legacy(-session) %>%
  # compute genders and store results in list-col
  mutate(first_name_gender = map(data,
                                 ~gender_safe(.x$`first-named speaker`)))

# Ignoring error list, just looking at the result list with gender
d_genders_unnest <-
  d_genders %>%
  mutate(speaker_gender = transpose(first_name_gender)[['result']]) %>%
  mutate(speaker_gender_chr = map(speaker_gender, ~mutate_all(.x, as.character))) %>%
  unnest_legacy(speaker_gender_chr) %>%
  select(session, gender, name)

return(genders_unnest = d_genders_unnest)

}


