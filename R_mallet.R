# Simple mallet implementation for topic modeling

library("stopwords")
library("mallet")
library('reticulate')
library('tidyverse')

# read csv
csv <- read_csv2("INPUT TOPIC MODEL CSV", col_names=TRUE)#, col_types= cols(.default = "?", ID = "c", created_at = "T"))
csv <- data.frame(csv)

file = csv$body

#export each row in csv
source_python("~/Dropbox/tm/rowwriter.py")

rowwriter(file, "DEFINE OUTPUT FOLDER")
#check state of output
x <- list.files(path = "ADD PATH OUTPUT FOLDER")

#run mallet
input <- mallet.read.dir("ADD PATH OUTPUT FOLDER")
tm <- mallet.import(input$id, input$text, "ADD STOPWORDS FILE")

#intialize mallet topicmodel
#define number of topics
topic.model <- MalletLDA(num.topics=10)

topic.model$loadDocuments(tm)

#tibble(word.freqs) %>% arrange(desc(tibble(word.freqs)$term.freq))

## Optimize hyperparameters every 20 iterations, after 50 burn-in iterations
topic.model$setAlphaOptimization(10, 50)

#train model set number of iterations
topic.model$train(1000)

topic.model$maximize(10)

#examine output
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
