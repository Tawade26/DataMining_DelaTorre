# install.packages(c("tm", "SnowballC", "wordcloud", "RColorBrewer"))

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# Part 1
# setwd("C:/Users/keith/OneDrive/Desktop/Hans_DM1/DM_WORDCLOUD")
feedback <- readLines("feedback.txt", encoding = "UTF-8", warn=FALSE)
feedback <- paste(feedback, collapse = " ")

corpus <- Corpus(VectorSource(feedback))

corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(function(x) gsub("[^a-z ]", " ", x)))
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, stemDocument)


# Part 2
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
df <- data.frame(word = names(word_freqs), freq = word_freqs)
print("Top 10 Most Frequent Words")                                 
head(df, 10)
# The most used words are "Mandatory", "Official", and "Services" indicating a requirement in the office field.
# It could also talks about the performance and services feedbacks.
# Other word such as "Officer", "Wait", "Staff", and "Clerk" indicated a user expereince feedback concerning about speed and efficieny.
# This insight could report back to as a potential improvements on Staff Performance and Queue Management.


# Part 3
png("wordcloud_exam.png", width = 800, height = 600)
set.seed(1126)

wordcloud(
  words = df$word,
  freq = df$freq,
  min.freq = 2, 
  max.words = 1000,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)
dev.off()


# Part 4
df_rare <- subset(df, freq==1)

print("Least Frequent Words")
head(df_rare, 5)


png("wordcloud_rare.png", width = 800, height = 600)
set.seed(0709)

wordcloud(
  words = df_rare$word,
  freq = df_rare$freq,
  min.freq = 1,
  max.words = 1000,
  random.order = FALSE,
  rot.per = 0.35,
  colors = brewer.pal(8, "Dark2")
)

dev.off()