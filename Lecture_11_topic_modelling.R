
## Course: R for Social Scientists 
## Instructor: Karina Shyrokykh 
## Term: VT2021

# Plan:
# 1. Topic modelling with LDA
# 2. Data filtering + tm LDA

rm( list=ls() )
getwd()
setwd("/Users/kash7423/Desktop/SU teaching/2021/VT21/Summer course in R/vt2021 slides code")

df <- fromJSON(paste(readLines('tweets_1week_climate.json'), collapse=""))

############ Topic models #####################
# For more see https://www.tidytextmining.com/topicmodeling.html
# https://juliasilge.github.io/tidytext/articles/topic_modeling.html

install.packages("topicmodels")
library(topicmodels)

library(tidytext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(tm)

# Create a document-term matrix (DTM)
custom_stopwords <- add_row(stop_words, word="ð", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="â", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="http", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="t.co", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="amp", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="ðÿ", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="climateemergency", lexicon="custom")

dtm <- df %>%
  mutate(text=removeNumbers(text)) %>% 
  mutate(text=gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", text)) %>%
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  mutate(word=wordStem(word)) %>%
  count(user_id, status_id, word, sort=T) %>% 
  cast_dtm(status_id, word, n) # transform a data frame to a Document Term Matrix

dtm # to examine our dtm
inspect(dtm[1:20, 1:15])

View(as.matrix(dtm[1:1000, 1:5])) # in a separate window

df$text[df$status_id=="1407832023822061570"] # print the tweet to examine if correct dtm

# Run the Latent Dirichlet allocation algorithm with k topics and a certain random generator seed
lda <- LDA(dtm, k = 2, control = list(seed = 500))

# Inspect: for each combination the model has beta, the probability of that term being generated from that topic.
top_terms <- tidy(lda) %>%
  group_by(topic) %>%
  top_n(5, beta) %>% # to find the 10 terms that are most common within each topic
  ungroup() %>%
  arrange(topic, -beta) # tidytext package allows extracting the per-topic-per-word probabilities, called beta

top_terms # "climatecrisi"  has a  0.0220 probability of being generated from topic 1
# but a 0.0254 probability of being generated from topic 2

# Plot term frequencies per topic
top_terms %>%
  mutate(term = reorder_within(term, -beta, topic)) %>%
  ggplot(aes(term, beta)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x") + 
  labs(title="Inherent topics") 


############ Working with dictionaries (aka lexicons) ###############
climate_dictionary_1 <- "(global)"

climate_dictionary_2 <- "(international|multilateral|agreement|negotiation|cop|summit|global)"

# Using dictionaries for filtering data
# let's count the number of tweets 
df_1 <- df %>%
  filter(
    str_detect(df$text, regex(climate_dictionary_1, ignore_case = T))
  )

nrow(df_1)
# how many tweets mention either of the two?

# let's count the number of tweets 
df_2 <- df %>%
  filter(
    str_detect(df$text, regex(climate_dictionary_2, ignore_case = T))
  )

nrow(df_2)



# Create a document-term matrix (DTM)
custom_stopwords <- add_row(stop_words, word="ð", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="â", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="http", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="t.co", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="amp", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="ðÿ", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="climateemergency", lexicon="custom")

dtm_1 <- df_1 %>%
  mutate(text=removeNumbers(text)) %>% 
  mutate(text=gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", text)) %>%
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  mutate(word=wordStem(word)) %>%
  count(user_id, status_id, word, sort=T) %>% 
  cast_dtm(status_id, word, n) # transform a data frame to a Document Term Matrix

dtm_1 # to examine our dtm
inspect(dtm_1[1:20, 1:15])

View(as.matrix(dtm_1[1:10, 1:5])) # in a separate window

df$text[df$status_id=="1407832023822061570"] # print the tweet to examine if correct dtm

# Run the Latent Dirichlet allocation algorithm with k topics and a certain random generator seed
lda <- LDA(dtm_1, k = 4, control = list(seed = 500))

# Inspect: for each combination the model has beta, the probability of that term being generated from that topic.
top_terms <- tidy(lda) %>%
  group_by(topic) %>%
  top_n(5, beta) %>% # to find the 10 terms that are most common within each topic
  ungroup() %>%
  arrange(topic, -beta) # tidytext package allows extracting the per-topic-per-word probabilities, called beta

top_terms 

# Plot term frequencies per topic
top_terms %>%
  mutate(term = reorder_within(term, -beta, topic)) %>%
  ggplot(aes(term, beta)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x") + 
  labs(title="Inherent topics") 

# the same procedure for df_2 based on a more complete filtering of global climate governance
dtm_2 <- df_2 %>%
  mutate(text=removeNumbers(text)) %>% 
  mutate(text=gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", text)) %>%
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  mutate(word=wordStem(word)) %>%
  count(user_id, status_id, word, sort=T) %>% 
  cast_dtm(status_id, word, n) # transform a data frame to a Document Term Matrix

dtm_2 # to examine our dtm
inspect(dtm_2[1:20, 1:15])

View(as.matrix(dtm_2[1:10, 1:5])) # in a separate window

# Run the Latent Dirichlet allocation algorithm with k topics and a certain random generator seed
lda <- LDA(dtm_2, k = 4, control = list(seed = 500))

# Inspect: for each combination the model has beta, the probability of that term being generated from that topic.
top_terms <- tidy(lda) %>%
  group_by(topic) %>%
  top_n(5, beta) %>% # to find the 10 terms that are most common within each topic
  ungroup() %>%
  arrange(topic, -beta) # tidytext package allows extracting the per-topic-per-word probabilities, called beta

top_terms 

# Plot term frequencies per topic
top_terms %>%
  mutate(term = reorder_within(term, -beta, topic)) %>%
  ggplot(aes(term, beta)) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  geom_bar(stat = "identity") +
  scale_x_reordered() +
  facet_wrap(~ topic, scales = "free_x") + 
  labs(title="Inherent topics") 
