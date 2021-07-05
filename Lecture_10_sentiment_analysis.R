
## Course: R for Social Scientists 
## Instructor: Karina Shyrokykh 
## Term: VT2021

# Plan:
# 1. Sentiment analysis 
# 2. Descriptive analysis of sentiments of texts 
# 3. How to create your own dictionary

getwd()

rm( list=ls() )
library("tidyverse")
setwd("/Users/kash7423/Desktop/SU teaching/2021/VT21/Summer course in R/vt2021 slides code")

df <- fromJSON(paste(readLines('tweets_1week_climate.json'), collapse=""))

##################  Descriptive analysis: bonus content (sentiment analysis) ######################################
## Example of analysis like in the book

# Examining the dataset
head(df)

# Prin the first 10 obs 
df %>% head(10)

# Show only text
df$text %>% head(10)

# sentiment analysis using Afinn lexicon (for more see documentation here: https://github.com/fnielsen/afinn)
sentiment_eng_raw <- read_table("https://raw.githubusercontent.com/fnielsen/afinn/master/afinn/data/AFINN-111.txt", col_name = "text")

# show the list
sentiment_eng_raw

# to seprate words from scores
sentiment_eng <- sentiment_eng_raw %>% 
  separate(text, c("word", "score"), sep = -2) %>%
  mutate(
    word = str_trim(word),
    score = as.integer(str_trim(score))
  ) %>%
  drop_na(score)

# what do you see?
sentiment_eng

# how many words are there? Are there any duplicates?
sentiment_eng %>% 
  distinct(word) %>% 
  nrow()

######################################
# to analyse sentiment analysis:
# 1. split the tokenize the corpus
# 2. download the lexicon/dictionary
# 3. attach a sentiment score to each word
#####################################


# examine the sentiment in the stop words
stopwords <-stop_words %>% 
  left_join(sentiment_eng, by = "word")

stopwords

stopwords %>% 
  ggplot(aes(x = score)) + 
  geom_bar() +
  labs (x = 'sentiment score', y = NULL,
        title = 'Sentiment in stop word dictionary',
        subtitle = 'Examination of the data that we drop')

# what do you see?
# How many rows did we drop due to NAs?
# how can you interpret the sentiment score? are the dropped words positive or negative?

# how many words had sentiment =="0"?
stopwords  %>% 
  filter (score == 0)  %>% 
  nrow()
# how many?


######################################
# Sentiment analysis of tweets
######################################

# Check lexicon words
get_sentiments("afinn")
get_sentiments("bing")
get_sentiments("nrc")
# how many words are in each?


# Step 4: Remove numbers
freq <-   df %>% 
  mutate(text=removeNumbers(text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  group_by(status_id) %>%
  inner_join(get_sentiments("afinn")) %>%
  summarise(score=sum(value))

ggplot(freq, aes(x = score)) + 
  geom_bar() +
  labs (x = 'sentiment score', y = 'number of tweets',
        title = 'Sentiment in tweets')

# 1. split the corpus into individual words
Encoding(df$text) <-"latin1"

df_words <- unnest_tokens(
  df,
  input = text, output = word,
  token = "words", to_lower = TRUE
)

df_words %>% head(10)

# combine the datasets
df_words <-df_words %>% 
  left_join(sentiment_eng, by = "word")

df_words %>% head(10)

# visualization
df_words %>% 
  ggplot(aes(x = score)) + 
  geom_bar() +
  labs (x = 'sentiment score', y = NULL,
        title = 'Sentiment in climate emergency tweets',
        subtitle = 'Examination of the data ')

##################  the end of the sentiment exercise ######################################

################## Word clouds  ######################################

df_words_count <- 
  df_words %>% 
  anti_join(custom_stopwords) %>% 
  mutate(word=wordStem(word)) %>% 
  count(word, score, sort = T) %>% 
  mutate(influence = score * n)

df_words_count %>% head(25)


### Largest positive impact
df_words_count %>%  
  arrange(desc(influence)) %>% 
  head(10)

# create tables for positive and negative words
df_words_count_positive <- df_words_count %>% 
  slice_max(influence, n=10) %>% 
  mutate(sentiment = "Positive")

df_words_count_negative <- df_words_count %>% 
  slice_min(influence, n=10) %>% 
  mutate(sentiment = "Negative")

# create a table with the two lists
df_words_count_pos_neg <- bind_rows(df_words_count_positive, df_words_count_negative)

# create a nice graph
df_words_count_pos_neg  %>% 
  ggplot(aes(x=reorder(word, influence),
             y = influence,
             fill = sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free") +
  coord_flip() +
  labs(title = "Negative and positive words with the greatest influence",
       x = "word",
       y = "influence")


# Word clouds 

installed.packages('reshape2')
install.packages('wordcloud')
install.packages('RColorBrewer')

library(reshape2)
library(wordcloud)
library(RColorBrewer)

wordcloud(df_words_count_pos_neg$word, df_words_count_pos_neg$count, max.words=40)


df_words_count_pos_neg  %>% 
  acast(formula = word ~ sentiment, 
        value.var = "influence", fill = 0) %>% 
  comparison.cloud(colors = c("grey", "black"),
                   max.words = 30)

############ Working with dictionaries (aka lexicons) ###############
climate_dictionary_1 <- "(global warming|climate change)"

climate_dictionary_2 <- "(warming|renewables|biodiversity|emissions|mitigation|green|climateact|nature)"

# Using dictionaries for filtering data
# let's count the number of tweets 
n_dict_1 <- df %>%
  filter(
    str_detect(df$text, regex(climate_dictionary_1, ignore_case = T))
  )

nrow(n_dict_1)
# how many tweets mention either of the two?

# let's count the number of tweets 
n_dict_2 <- df %>%
  filter(
    str_detect(df$text, regex(climate_dictionary_2, ignore_case = T))
  )

nrow(n_dict_2)
# how many tweets mention either of the two?
library(tm)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

custom_stopwords <- add_row(stop_words, word="ð", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="â", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="http", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="t.co", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="amp", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="ðÿ", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="climateemergency", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="climate", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="change", lexicon="custom")
freq <- n_dict_1 %>% 
  mutate(text=removeNumbers(text)) %>% 
  mutate(text=gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  mutate(word=wordStem(word)) %>% 
  count(word, sort=T) %>% 
  top_n(10, n)

wordcloud(words = freq$word, freq = freq$n, min.freq = 5,
          max.words=70, # max.words Maximum number of words to be plotted. least frequent terms dropped
          random.order=T, #plot words in random order. If false, they will be plotted in decreasing frequency; 
          rot.per=0.2, #proportion words with 90 degree rotation
          colors=brewer.pal(8, "Dark2"))


# for the secons set
custom_stopwords <- add_row(stop_words, word="ð", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="â", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="http", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="t.co", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="amp", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="ðÿ", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="climateemergency", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="climate", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="change", lexicon="custom")
freq_2 <- n_dict_2 %>% 
  mutate(text=removeNumbers(text)) %>% 
  mutate(text=gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  mutate(word=wordStem(word)) %>% 
  count(word, sort=T) %>% 
  top_n(10, n)


wordcloud(words = freq_2$word, freq = freq$n, min.freq = 5,
          max.words=70, # max.words Maximum number of words to be plotted. least frequent terms dropped
          random.order=T, #plot words in random order. If false, they will be plotted in decreasing frequency; 
          rot.per=0.2, #proportion words with 90 degree rotation
          colors=brewer.pal(8, "Dark2"))

