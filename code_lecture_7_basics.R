## Course: R for Social Scientists 
## Instructor: Karina Shyrokykh 
## Term: VT2021

# Plan:
# 1. Basic text analysis
# 2. Twitter data retrieval 

####################  Basics about text as data #################### ####################  
rm( list=ls() )

library("tidyverse")

getwd()
setwd("/Users/kash7423/Desktop/SU teaching/2021/VT21/Summer course in R/vt2021 slides code")

# Examples:
string_test <- "This is  hfhhf kkkd allslls s kkkks a test" # created an object

cat(string_test) # to examine the object

string_test # this is another way to examine an object

print(string_test) # a third way

# Text has a format "character".
# We can combine both textual and quantitative data in one document
revenues_SEK <- c(200, 440)
item_sold <- c("PC","Iphone")
sales <- tibble(revenues_SEK, item_sold)
sales

# Converting characters to numbers
numbers <- c("33", "44")
mean(numbers)  # Why do we see an error?

numbers <- as.numeric(numbers)
mean(numbers) # What do you see now?

longer_text <- "University Stockholm University has about 29,300 students (full-time equivalents), 1,400 doctoral students, and 5,700 members of staff. Total revenues amounted in 2020 to 5,45 billion SEK. Find more facts and figures here."

longer_text

longer_text_sentences <- c("University Stockholm University has about 29,300 students (full-time equivalents), 1,400 doctoral students, and 5,700 members of staff.", 
                           "Total revenues amounted in 2020 to 5,45 billion SEK.", 
                           "Find more facts and figures here.")

longer_text_sentences # what the indexes in the beginning of the lines mean?

# to see the second sentence
longer_text_sentences[2]

# print the second and the third element 
longer_text_sentences[2:3]

# to print the content of longer_text_sentences as a text
paste(longer_text_sentences, collapse = " ")

# create a dataframe
sentences_df <- tibble(sentence = 1:3, text = longer_text_sentences)
sentences_df

# create a dataframe limited to sentences 2 and 3
sentences_df <- tibble(sentence = 1:4, text = longer_text_sentences)
sentences_df # what does the error mean? explain

# how many characters are in the text:
str_length(longer_text)
longer_text
# number of characters per sentence (by sentences)
str_length(longer_text_sentences)  
str_length(sentences_df$text)


# How many characters each of the sentence has in a graph:
sentences_df %>%
  mutate(count = str_length(text)) %>% 
  ggplot(aes(x = sentence, y = count)) +
  geom_col() +
  coord_flip()


# which of the sentences has word "students"
sentences_df$text %>%
  str_detect("University") # what do you see? what does it mean?
sentences_df
# how many times  "students" are mentioned
sentences_df$text %>%
  str_count("and")

# NB: the role of uppercase 
sentences_df$text %>%
  str_count("students")

sentences_df$text %>%
  str_count("Students")

# to deal with uppercase:
sentences_df$text %>%
  str_count("[Ss]tudents") # count both "Students" and "students"

# count only characters and not blank spaces or punctuation
sentences_df$text %>%
  str_count("\\w")

# count the number of words
sentences_df$text %>%
  str_count("\\w+")

# let's illustrate the average number of characters in a word:
sentences_plot <- sentences_df %>%
  mutate(characters = str_count(text, "\\w"), 
         words = str_count(text, "\\w+"),
         charword = characters / words)

ggplot(sentences_plot, aes (x = sentence, y = charword)) +
  geom_col() +
  coord_flip()

sentences_df

# plot the number of words
sentences_plot <- sentences_df %>%
  mutate(words = str_count(text, "\\w+"))

ggplot(sentences_plot, aes (x = sentence, y = words)) +
  geom_col() +
  coord_flip()

sentences_df

# count the number of words that contain letter "s". 
sentences_df$text %>%
  str_count("\\w*s+\\w*")

sentences_df # is all ok? was it correct


#Try now. What is the difference between the two, why?
sentences_df$text %>%
  str_count("\\w*[Ss]+\\w*")

# Some logical operators for texts: see p. 267 in the RfS textbook

# to find all symbols that are not letters
sentences_df$text %>%
  str_count("\\W")

# to find all digits
sentences_df$text %>%
  str_count("\\d")

# to find all symbols that are not digits
sentences_df$text %>%
  str_count("\\D")

# to find all letters and digits (the same as \\w)
sentences_df$text %>%
  str_count("[:alnum:]")

# to find all small letters 
sentences_df$text %>%
  str_count("[:lower:]")

# to find all capital letters 
sentences_df$text %>%
  str_count("[:upper:]")

# ... etc.

# ignore whether it is an upper or lower case
sentences_df$text %>%
  str_count(regex("students", ignore_case = TRUE))

# Split a text at a particular place
sentences_df$text %>%
  str_split("and")

# Substitute a word by another word ("and" by "as well as")
sentences_df$text %>%
  str_replace_all("and", "as well as")

########## ########### The end of the introduction. Next: Twitter data retrieval (the real stuff) #################



#################### ####################  Step 1. Data retrieval: Twitter #################### #################### 
# There are different ways to obtain data:
# 1. Using Twitter API (relatively unlimited access). 
# 2. Using R packages (limited access)
# 3. Use someone else's data (e.g., on GitHub)

## Good material:
# On package rtweet: https://www.infoworld.com/article/3515712/how-to-search-twitter-with-rtweet-and-r.html
# On Twitter API:  https://mkearney.github.io/nicar_tworkshop/#47

updateR()

install.packages("rtweet")
install.packages("tidyverse")
install.packages("tidytext")
install.packages("installr")
install.packages("colorspace")

library(installr)
library(rtweet) 
library(jsonlite)

# If there aren’t credentials stored on your system, a browser window should open asking you to authorize the request. 
# After that, an authorization token will be stored in your .Renviron file so you don’t have to re-authorize in the future.
tweets <- search_tweets(q = "#ClimateEmergency", 
                        n = 10000,
                        include_rts = FALSE,
                        lang = "en")

write_json(tweets, 'tweets_1week_climate.json')

tweets_2 <- search_tweets(q = "#actonclimate", 
                        n = 10000,
                        include_rts = FALSE,
                        lang = "en")

write_json(tweets_2, 'tweets_1week_actonclimate.json')

#################### ####################  Step 2: Download & clean the data #################### ####################  

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

##################### 2.1. Some basic data examination###################
# Detect location of tweets
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

# Option 2: pre-processing -- to lower (see below)

# To lower might be not the only thing that we want to do:
# Frequent words
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
