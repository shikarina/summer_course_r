# Twitter retrieval beyond "rtweet"

# Option 1: Twitter API -- Acdemic track

install.packages("academictwitteR")
library("academictwitteR")
library("curl")

bearer_token <- "your_token"

# 
# academictwitteR allows to oversome the issue of limited access to Twitter data (500 tweets at once) via Twitter API
# for more on limitations per request see https://developer.twitter.com/en/docs/twitter-api/tweets/search/quick-start/full-archive-search
users <- c("account_name")
tweets <-
  get_user_tweets(users,
                  "2009-01-01T00:00:00Z",
                  "2020-01-01T00:00:00Z",
                  bearer_token)
library("jsonlite")
write_json(tweets, 'data.json')

df <- fromJSON(paste(readLines('data.json'), collapse=""))

# Option 2: Twitter API 

library(httr)

# Replace the bearer token below with
bearer_token = "your_token"

headers = c(
  'Authorization' = sprintf('Bearer %s', bearer_token)
)

# set parameters of your query
params = list(
  'query' = 'from:account_name lang:en', # replace "account_name"
  'start_time' = "2009-01-01T00:00:00Z",
  'end_time' = "2020-01-01T00:00:00Z",   'max_results' = '500',
  'tweet.fields' = 'attachments,author_id,context_annotations,created_at,entities,geo,id,in_reply_to_user_id,lang,possibly_sensitive,public_metrics,referenced_tweets,source,text,withheld'
)

response <- httr::GET(url = 'https://api.twitter.com/2/tweets/search/all', httr::add_headers(.headers=headers), query = params)


fas_body <-
  content(
    response,
    as = 'parsed',
    type = 'application/json',
    simplifyDataFrame = TRUE
  )

install.packages("jsonlite")
library("jsonlite")
View(fas_body$data)
write_json(fas_body$data, 'data.json')

json_data <- fromJSON(paste(readLines('data.json'), collapse=""))
 

