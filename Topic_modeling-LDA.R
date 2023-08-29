#this album takes a corpus of timestamped 'topics' (e.g. newspaper-headlines, or even, Twitter-usernames),
#and provides an output over which topics are the most dominant for the respective dataset.
#In combination with script "", this procedure provides an output of which topics were most popular per timestep (e.g. day / week 8 month). 
#The overall method is based on Freelon et al. (2018), to measure social media power.

topic_modeling <- function(filepath, alpha=NULL){
  #prepare stopwords
  english_stopwords <- readLines("REPLACE WITH STOPWORD FILE", encoding = "UTF-8")
  
  #create corpus
  filepath <- file.path(filepath)
  docs <- Corpus(DirSource(filepath))
  #docs <- tokens(docs, remove_separators = TRUE)
  
  #process corpus
  processedCorpus <- tm_map(docs, content_transformer(tolower))
  processedCorpus <- tm_map(processedCorpus, removeWords, english_stopwords)
  processedCorpus <- tm_map(processedCorpus, removePunctuation, preserve_intra_word_dashes = TRUE)
  processedCorpus <- tm_map(processedCorpus, removeNumbers)
  processedCorpus <- tm_map(processedCorpus, stemDocument, language = "en")
  processedCorpus <- tm_map(processedCorpus, stripWhitespace)
  
  
  # compute document term matrix with terms >= minimumFrequency
  minimumFrequency <- 5
  #create dtm
  DTM <- DocumentTermMatrix(processedCorpus, control = list(bounds = list(global = c(minimumFrequency, Inf))))
  #remove empty fields
  sel_idx <- slam::row_sums(DTM) > 0
  DTM <- DTM[sel_idx, ]
  
  #Set parameters for Gibbs sampling
  burnin <- 1000
  iter <- 7000
  seed <-list(2003,5,63,100001,765)
  nstart <- 5
  best <- TRUE

  #Number of topics
  k <- 8
  
  if(!is.null(alpha)) {
  #Run LDA using Gibbs sampling
    ldaOut2 <<-LDA(DTM, k, method= 'Gibbs', control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, verbose= 50, alpha=alpha))
    
    #write out results
    #docs to topics
    ldaOut2.topics <<- as.matrix(topics(ldaOut2))
    
    topics_collector <- as.data.frame(ldaOut2.topics)
    topics_collector$documents <- rownames(ldaOut2.topics)
    index <- c(1:max(nrow(ldaOut2.topics)))
    rownames(topics_collector) <- index
    topics_collector <<- topics_collector  
    }
  else{
    ldaOut <<-LDA(DTM, k, method= 'Gibbs', control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, verbose= 50))
    #Run LDA using Gibbs sampling
    
    #write out results
    #docs to topics
    ldaOut.topics <<- as.matrix(topics(ldaOut))
    
    topics_collector <- as.data.frame(ldaOut.topics)
    topics_collector$documents <- rownames(ldaOut.topics)
    index <- c(1:max(nrow(ldaOut.topics)))
    rownames(topics_collector) <- index
    topics_collector <<- topics_collector
  }
  
  #merge iterations over multiple weeks
  #topics_collector <- rbind(topics_collector, ldaOut.topics)
  topics_df <- topics_collector
  topics_df <- topics_df %>% dplyr::group_by(V1)
  topics_df <<- group_split(topics_df)
}
