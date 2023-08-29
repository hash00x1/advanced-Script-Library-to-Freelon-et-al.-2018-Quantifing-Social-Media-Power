library(tidyverse)
library(clean)
library(RedditExtractoR)

###Mutate reddit_csv data from csv
reddit_csv <- read_csv("read.csv", col_names=TRUE)#, col_types= cols(.default = "?", ID = "c" , created_at = "T", user_id ="c"))

#pull reddit data from API
#get list of links
links <- reddit_urls(subreddit = "DEFINE SUBREDDIT TO PULL DATA", page_threshold = 10, cn_threshold = 5)
links <- as_tibble(links)
links <- links %>% arrange(desc(num_comments))
url <- links[1:100, "URL"]

#filter tibble
#filter <- url %>% filter(row_number() == (n() - 1))

#get full thread data from reddit
reddit_data <- tibble()
for (i in url){
  entry <- reddit_content(i)
  reddit_data <- rbind(reddit_data, entry)}
reddit_data <- as_tibble(reddit_data)

reddit_source <- get_reddit(subreddit = "uberdrivers", cn_threshold = 5, page_threshold = 10)
reddit_df <- as_tibble(reddit_source)

#caculate thread_frequency
thread_frequency <- freq(tweets_csv$thread_id)

#caculate top_frequency
top_thread_frequency <- top_freq(thread_frequency)
