
## Course: R for Social Scientists 
## Instructor: Karina Shyrokykh 
## Term: VT2021

# Plan:
# 1. Descriptive analysis: word frequency visualization (histogram and word clouds), compare accounts in a histogram, line graph (over time)
# 2. Bonus content: Sentiment analysis 

rm( list=ls() )

getwd()
setwd("/Users/kash7423/Desktop/SU teaching/2021/VT21/Summer course in R/vt2021 slides code")

df <- fromJSON(paste(readLines('tweets_1week_climate.json'), collapse=""))

library(tidyverse)
library(installr)
library(rtweet) 
library(jsonlite)
library(tidyverse)
library(jsonlite)
library(tidytext)
library(SnowballC)
library(dplyr)
library(tm)
library(ggplot2)

############## Descriptive examination of the debate  ######################################

# Histogram of the most frequent words (which we already built)
custom_stopwords <- add_row(stop_words, word="ð", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="â", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="http", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="t.co", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="amp", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="ðÿ", lexicon="custom")
freq <- df %>% 
  mutate(text=removeNumbers(text)) %>% 
  mutate(text=gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  mutate(word=wordStem(word)) %>% 
  count(word, sort=T) %>% 
  top_n(10, n)

# Plot histogram
ggplot(freq, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  labs(title="Twitter ", x = NULL, y = "Frequency") +
  coord_flip()

# Word cloud of the most frequent words

install.packages("wordcloud")
install.packages("RColorBrewer")
install.packages("wordcloud2")
install.packages("tm")

library(tm)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)


freq <- df %>% 
  mutate(text=removeNumbers(text)) %>% 
  mutate(text=gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", text)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  mutate(word=wordStem(word)) %>% 
  count(word, sort=T)

wordcloud(words = freq$word, freq = freq$n, min.freq = 10,
          max.words=50, # max.words Maximum number of words to be plotted. least frequent terms dropped
          random.order=T, #plot words in random order. If false, they will be plotted in decreasing frequency; 
          rot.per=0.1, #proportion words with 90 degree rotation
          colors=brewer.pal(8, "Dark2"))


# We can also compare accounts and build histograms for each account
# Comparing accounts (cf. ch 9, p. 229)

tweet_freq <- df  %>% 
  group_by(name)  %>% 
  summarise(n_total = n_distinct(status_id)) %>%
  arrange(desc(n_total)) %>%
  # count(status_id, sort=T) %>% 
  top_n(10, n_total)
# Plot histogram
ggplot(tweet_freq, aes(x = reorder(name, n_total), y = n_total)) +
  geom_col() +
  labs(title="Most tweeting accounts", x = "Account", y = "Number of tweets") +
  coord_flip()

# Let's see 3 most active accounts
tweet_freq_select <- df  %>% 
  group_by(name, user_id)  %>% 
  summarise(n_total = n_distinct(status_id)) %>%
  arrange(desc(n_total)) %>%
  top_n(3, n_total)

tweet_freq_select # who tweet the most?


# Let's clean the data
custom_stopwords <- add_row(stop_words, word="ð", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="â", lexicon="custom")
custom_stopwords <- add_row(stop_words, word="http", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="t.co", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="amp", lexicon="custom")

twitter_words <- df %>%
  mutate(text=removeNumbers(text)) %>% 
  mutate(text=gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", text)) %>%
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  mutate(word=wordStem(word)) %>% 
  count(user_id, status_id, word, sort=T) 

twitter_idf <- twitter_words %>%
  bind_tf_idf(word, status_id, n) %>%
  arrange(desc(tf_idf))

# Plot histograms of the most frequent words used by the most active users employing tf-idf statistic

# User #1
twitter_idf_freq1 <- twitter_idf %>% 
  filter(user_id==as.numeric(tweet_freq_select[1, "user_id"])) %>%
  distinct(word, .keep_all = TRUE) %>%
  arrange(desc(tf_idf)) %>%
  top_n(20, tf_idf)
# Plot frequencies
plot1 <- ggplot(twitter_idf_freq1, aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col() +
  labs(title=toString(tweet_freq_select[1, "name"]), x = "Word", y = "TF-IDF score") +
  coord_flip()

# User #2
twitter_idf_freq2 <- twitter_idf %>% 
  filter(user_id==as.numeric(tweet_freq_select[2, "user_id"])) %>%
  distinct(word, .keep_all = TRUE) %>%
  arrange(desc(tf_idf)) %>%
  top_n(20, tf_idf) 
# Plot frequencies
plot2 <- ggplot(twitter_idf_freq2, aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col() +
  labs(title=toString(tweet_freq_select[2, "name"]), x = "Word", y = "TF-IDF score") +
  coord_flip()

# User #3
twitter_idf_freq3 <- twitter_idf %>% 
  filter(user_id==as.numeric(tweet_freq_select[3, "user_id"])) %>%
  distinct(word, .keep_all = TRUE) %>%
  arrange(desc(tf_idf)) %>%
  top_n(20, tf_idf) 
# Plot frequencies
plot3 <- ggplot(twitter_idf_freq3, aes(x = reorder(word, tf_idf), y = tf_idf)) +
  geom_col() +
  labs(title=toString(tweet_freq_select[3, "name"]), x = "Word", y = "TF-IDF score") +
  coord_flip()

# Single plot for all three users
library(gridExtra)
grid.arrange(plot1, plot2, plot3, ncol = 3)

######################### over time dynamics  ######################################
install.packages("lubridate")
library(lubridate)

df %>%
  filter(user_id %in% as.numeric(unlist(as.list(tweet_freq_select[1:3, "user_id"])))) %>%
  mutate(created_at = parse_date_time(created_at ,"%y-%m-%d %H:%M:%S"),
         date = as_date(created_at)) %>%
  group_by(name, date) %>%
  summarize(n_tweets=n()) %>%
  ggplot(aes(date, n_tweets, color=name)) + 
  geom_line() +
  scale_x_date(date_breaks = "1 day", date_labels = "%y-%m-%d") +
  labs(color="Account")

######################### hashtags analysis  ######################################

# tokenize the data
df_words_hashtag <- df %>%
  unnest_tokens(output = word,
                input = text,
                token = "tweets",
                to_lower = T)

# create a column "word" which contains hashtags  for each tweet 
# if a tweet contails a few hashtags, it will create one row for each hashtag
df_words_hashtag_df <- df_words_hashtag %>%
  filter( str_starts(word, "#"),
         word != "#climateemergency")

df_words_hashtag_df$word %>% head(20)   

# number of hashtags in the table?
df_words_hashtag_df %>%
  distinct(status_id) %>%
  nrow()

# how many users
df_words_hashtag_df %>%
distinct(user_id) %>%
  nrow()

# Hashtags in the data
hashtag_freq <- df_words_hashtag_df  %>%
  count(word)  %>%
  arrange(desc(n))

# show 10 most common
hashtag_freq %>%
  head(10)

hashtag_freq %>%
  slice(1:10) %>%
  ggplot(aes(x=reorder(word, n), y= n)) +
  geom_col()+
  coord_flip()

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
custom_stopwords <-custom_stopwords %>% 
  left_join(sentiment_eng, by = "word")

custom_stopwords

custom_stopwords %>% 
  ggplot(aes(x = score)) + 
  geom_bar() +
  labs (x = 'sentiment score', y = NULL,
        title = 'Sentiment in stop word dictionary',
        subtitle = 'Examination of the data that we drop')

# what do you see?
# How many rows did we drop due to NAs?
# how can you interpret the sentiment score? are the dropped words positive or negative?

# how many words had sentiment =="0"?
custom_stopwords  %>% 
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

# creatre a nice graph
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

