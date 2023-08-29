#Simple topic modeling script to go over a corpus of single-file text-files and provide a tm-output.
#script contains a section to output topic-modeling results as bar-graph.

#import packages and scripts
library('ldaOuts')
library('tm')
library('ldaOuts')
library('reshape2')
library('ggplot2')
library('lda')
library('reshape2')
library('MASS')

topic_modeling <- source("") #define corpus source

#set source- filepath
filepath <- "REPLACE WITH FILEPATH"

#run ldaOut
topic_modeling(filepath)

#analyze results
#visualize top 10 terms per topic
terms(ldaOut2, 10)

#create result-overview
tmResult <- posterior(ldaOut2)
# format of the resulting object
attributes(tmResult)

#assign topicNames
top5termsPerTopic <- terms(lda_uber_n8, 20)
topicNames <- apply(top5termsPerTopic, 2, paste, collapse=" ")

# topics are probability distribtions over the entire vocabulary
beta <- tmResult$terms   # get beta from results
dim(beta)
# for every document we have a probaility distribution of its contained topics
theta <- tmResult$topics 
dim(theta) 

##VISUALIZE TOPIC DISTRIBUTIONS PER DOCUMENT
exampleIds <- seq(1, 10)

N <- length(exampleIds)
# get topic proportions from sample documents
topicProportionExamples <- theta[exampleIds,]
colnames(topicProportionExamples) <- topicNames
vizDataFrame <- melt(cbind(data.frame(topicProportionExamples), document = factor(1:N)), variable.name = "topic", id.vars = "document")  
ggplot(data = vizDataFrame, aes(topic, value, fill = document), ylab = "proportion") + 
  geom_bar(stat="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +  
  coord_flip() +
  facet_wrap(~ document, ncol = N)

# re-rank top topic terms for topic names
topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")
