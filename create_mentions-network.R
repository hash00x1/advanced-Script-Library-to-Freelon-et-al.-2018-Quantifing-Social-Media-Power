#create tweets-mentions network, based on .csv-formatted Twitter API v01 export.

mg <- mentions_grouped
names(mg) <- tweets_csv$ID[1:100]

#unlist list but keep indices
mg1 <- mg %>% enframe %>%
  unnest

#add colnames
colnames(mg1) <- c("ID", "mention_to_user_name")

#create mentions_csv
tweets_mentions <- dplyr::inner_join(tweets_csv, mg1, by = "ID")

