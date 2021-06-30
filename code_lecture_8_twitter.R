## Course: R for Social Scientists 
## Instructor: Karina Shyrokykh 
## Term: VT2021

# Plan:
# 1. Twitter data retrieval with rtweet
# 2. Data cleaning

#################### ####################  Step 1. Data retrieval: Twitter #################### #################### 
# There are different ways to obtain data:
# 1. Using Twitter API (relatively unlimited access). 
# 2. Using R packages (limited access)
# 3. Use someone else's data (e.g., on GitHub)

## Good material:
# On package rtweet: https://www.infoworld.com/article/3515712/how-to-search-twitter-with-rtweet-and-r.html
# On Twitter API:  https://mkearney.github.io/nicar_tworkshop/#47

#updateR()

rm( list=ls() )

getwd()
setwd("/Users/kash7423/Desktop/SU teaching/2021/VT21/Summer course in R/vt2021 slides code")

#install.packages("rtweet")
#install.packages("tidyverse")
#install.packages("tidytext")
#install.packages("installr")
#install.packages("colorspace")

library(installr)
library(rtweet) 
library(jsonlite)

# to be able to use rtweet you need to have a tweeter account and have it open at the time of running this query. 
# Retrieve data with rtweet. For more read https://cran.r-project.org/web/packages/rtweet/rtweet.pdf
tweets <- search_tweets(q = "#ClimateEmergency", 
                        n = 10000, # Specifies the number of direct messages to retrieve, max 10000
                        include_rts = FALSE, # whether to include retweets in search results.
                        lang = "en")

write_json(tweets, 'tweets_1week_climate.json')

tweets_2 <- search_tweets(q = "#actonclimate", 
                          n = 10000,
                          include_rts = FALSE,
                          lang = "en")

write_json(tweets_2, 'tweets_1week_actonclimate.json')

#################### ####################  Step 2: Download & clean the data #################### ####################  

install.packages("jsonlite")
install.packages("SnowballC")
install.packages("tm")

library(tidyverse)
library(jsonlite)
library(tidytext)
library(SnowballC)
library(dplyr)
library(tm)
library(ggplot2)

# Get dataframe with tweets
df <- fromJSON(paste(readLines('tweets_1week_climate.json'), collapse=""))
df_2 <- fromJSON(paste(readLines('tweets_1week_actonclimate.json'), collapse=""))

# You can download the data file from Athena into your computer and then into R too:
df_3 <- fromJSON(paste(readLines('tweets_3.json'), collapse=""))

##################### 2.1. Some basic data examination###################
# Detect location of tweets (indicated)
df %>% 
  count(location) %>% 
  arrange(desc(n)) 

# any na for screen_name?
anyNA(df$screen_name)

# display first 10 obs
df %>%
  select(text) %>%
  head(10)

# how many tweets contain word "crisis" 
df$text %>%
  str_detect("crisis") %>%
  sum()

# count the number of tweets which contain "C/crisis"
df$text %>%
  str_detect("[Cc]risis") %>%
  sum()

# is there any difference in the last two numbers, why?
# What can we do to avoid such a situation?

# Option 1:
df$text %>%
  str_detect(regex("crisis", ignore_case = T) ) %>%
  sum()

# Option 2: the REAL pre-processing 

# To lower might be not the only thing that we want to do:
# Let's see why. Try a frequency of words
freq <- df %>% 
  unnest_tokens(word, text) %>% # Split a column into tokens (here: words)
  count(word, sort=T) %>% 
  top_n(10, n)
# Plot histogram
ggplot(freq, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  labs(title="Twitter ", x = NULL, y = "Frequency") +
  coord_flip()

# What do you see? What can you say about the data? What can we do to obtain meaningful results? 
# Next: Data pre-processing

# Step 1: Remove stopwords
freq <- df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(stop_words) %>% 
  count(word, sort=T) %>% 
  top_n(10, n)
# Plot histogram
ggplot(freq, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  labs(title="Twitter ", x = NULL, y = "Frequency") +
  coord_flip()

# What do you see? What can we do to improve the corpus?

# Step 2: Customize the stopwords list. Add more stopwords & remove
custom_stopwords <- add_row(stop_words, word="https", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="amp", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="t.co", lexicon="custom")
custom_stopwords <- add_row(custom_stopwords, word="change", lexicon="custom")
freq <- df %>% 
  unnest_tokens(word, text) %>% 
  anti_join(custom_stopwords) %>% 
  count(word, sort=T) %>% 
  top_n(10, n)

# Plot histogram
ggplot(freq, aes(x = reorder(word, n), y = n)) +
  geom_col() +
  labs(title="Twitter ", x = NULL, y = "Frequency") +
  coord_flip()

# What do you see now? Any changes? Can you interpret the results?

# Now build the same graph but for 15 most frequent words.

# Step 3: Apply stemming
freq <- df %>% 
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

# What do you see?


# Step 4: Remove numbers (if needed)
freq <- df %>% 
  mutate(text=removeNumbers(text)) %>% 
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

# Step 5: Remove URLs
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

###### the end of data pre-processing ########

# Now try the same steps with your df_2 
# 1. what are the most "active" locations?
# 2. what are the most frequent words?
# 3. Apply the data pre-processing steps and examine the results after each step
