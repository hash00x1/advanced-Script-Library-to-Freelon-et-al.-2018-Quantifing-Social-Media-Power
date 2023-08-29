#create top hashtag timelines and compares them between two factions (e.g. group 1 & 2)
#unlist hashtags

##group1
group1_hashtags_grouped <- list()
for (i in group1_tweets$hashtags){
  x <- (unlist(strsplit(i, ",")))
  group1_hashtags_grouped <<- append(group1_hashtags_grouped, list(x))
}
names(group1_hashtags_grouped) <- group1_tweets$ID
#unlist list but keep indices
group1_hashtags_grouped <- group1_hashtags_grouped %>% enframe %>%
  unnest
colnames(group1_hashtags_grouped) <- c("ID", "hashtag")

#create mentions_csv
group1_hashtags <- dplyr::inner_join(group1_tweets, group1_hashtags_grouped, by = "ID")


##group2
group2_hashtags_grouped <- list()
for (i in group2_tweets$hashtags){
  x <- (unlist(strsplit(i, ",")))
  group2_hashtags_grouped <<- append(group2_hashtags_grouped, list(x))
}
names(group2_hashtags_grouped) <- group2_tweets$ID
#unlist list but keep indices
group2_hashtags_grouped <- group2_hashtags_grouped %>% enframe %>%
  unnest
colnames(group2_hashtags_grouped) <- c("ID", "hashtag")

#create mentions_csv
group2_hashtags <- dplyr::inner_join(group2_tweets, group2_hashtags_grouped, by = "ID")

#######
#######

##NEXT STEPS::
#Filter by day
#group1
tm_group1 <- group1_hashtags %>% dplyr::mutate(created_at = lubridate::floor_date(created_at, unit = "day")) %>% dplyr::group_by(created_at)
tm_group1_top <- group_split(tm_group1)

#group2
tm_group2 <- group2_hashtags %>% dplyr::mutate(created_at = lubridate::floor_date(created_at, unit = "day")) %>% dplyr::group_by(created_at)
tm_group2_top <- group_split(tm_group2)

#filter for hashtags
#>>list top hashtags per day

#group1
hashtags <- as_tibble()
for (i in tm_group1_top){
  x <- i %>%
      filter(hashtag != "NA")  %>%
          count(gsub(" ", "", tolower(hashtag)), sort = TRUE) #%>%
            #print(top_n(10))
  date <- as_tibble(i$created_at[0:nrow(x)])
  names(x) <- c("hashtag", "count")
  names(date) <- c("date")
  date <- cbind(x, date)
  hashtags <<- rbind(hashtags, date)
  }


write.table(hashtags, file = paste0(filepath, "hashtags","_overview", ".csv"), row.names = FALSE, col.names = TRUE, sep = ",")




