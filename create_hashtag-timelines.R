#create top hashtag timelines
#unlist hashtags

##REPS
reps_hashtags_grouped <- list()
for (i in reps_tweets$hashtags){
  x <- (unlist(strsplit(i, ",")))
  reps_hashtags_grouped <<- append(reps_hashtags_grouped, list(x))
}
names(reps_hashtags_grouped) <- reps_tweets$ID
#unlist list but keep indices
reps_hashtags_grouped <- reps_hashtags_grouped %>% enframe %>%
  unnest
colnames(reps_hashtags_grouped) <- c("ID", "hashtag")

#create mentions_csv
reps_hashtags <- dplyr::inner_join(reps_tweets, reps_hashtags_grouped, by = "ID")


##DEMS
dems_hashtags_grouped <- list()
for (i in dems_tweets$hashtags){
  x <- (unlist(strsplit(i, ",")))
  dems_hashtags_grouped <<- append(dems_hashtags_grouped, list(x))
}
names(dems_hashtags_grouped) <- dems_tweets$ID
#unlist list but keep indices
dems_hashtags_grouped <- dems_hashtags_grouped %>% enframe %>%
  unnest
colnames(dems_hashtags_grouped) <- c("ID", "hashtag")

#create mentions_csv
dems_hashtags <- dplyr::inner_join(dems_tweets, dems_hashtags_grouped, by = "ID")

#######
#######

##NEXT STEPS::
#Filter by day
#reps
tm_reps <- reps_hashtags %>% dplyr::mutate(created_at = lubridate::floor_date(created_at, unit = "day")) %>% dplyr::group_by(created_at)
tm_reps_top <- group_split(tm_reps)

#dems
tm_dems <- dems_hashtags %>% dplyr::mutate(created_at = lubridate::floor_date(created_at, unit = "day")) %>% dplyr::group_by(created_at)
tm_dems_top <- group_split(tm_dems)

#filter for hashtags
#>>list top hashtags per day

#reps
hashtags <- as_tibble()
for (i in tm_reps_top){
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




