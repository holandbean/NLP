library(tidyverse)
library(data.table)
data <- fread("~/Documents/advanced business/exam/Womens Clothing E-Commerce Reviews.csv", sep=",", header=T, strip.white = T, na.strings = c("NA","NaN","","?")) 
summary(data)


#install.packages("tidytext", repos = "https://cran.r-project.org")
library(tidytext)
library(dplyr)
text_df<-data_frame(row=data$`Clothing ID`, text=data$`Review Text`)
head(text_df)

#tokenlize
tidy_text <- text_df %>%
  unnest_tokens(word, text)


#remove stopwords
data(stop_words)
tidy_text <- tidy_text %>%
  anti_join(stop_words,by='word')

#count word
tidy_text %>% count(word, sort = TRUE) 

#visualization
library(ggplot2)
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 4000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#stemming
#install.packages("SnowballC", repos = "https://cran.r-project.org")
library(SnowballC)

tidy_text <- data %>%
  unnest_tokens(word, text) %>%
  mutate(word = wordStem(word)) 

#remove stop words
tidy_text <- tidy_text %>%
  anti_join(stop_words,by='word')

#count word
tidy_text %>%
  count(word, sort = TRUE) 

#visualization
library(ggplot2)
tidy_text %>%
  count(word, sort = TRUE) %>%
  filter(n > 6000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  xlab(NULL) +
  coord_flip()

#word cloud
library(wordcloud)
tidy_text %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 200))

#colored word cloud
install.packages("reshape2", repos = "https://cran.r-project.org")
library(reshape2)
tidy_text %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)

#topic_modeling

library(RTextTools)
library(tm)
library(wordcloud)
library(topicmodels)
library(slam)
data <- data[1:1000,] # We perform LDA on the rows 1 through 1000 in the data.
corpus <- Corpus(VectorSource(data$answers), readerControl=list(language="en"))
dtm <- DocumentTermMatrix(corpus, control = list(stopwords = TRUE, minWordLength = 2, removeNumbers = TRUE, removePunctuation = TRUE,  stemDocument = TRUE))
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ] #remove all docs without words
lda <- LDA(dtm.new, k = 14) # k is the number of topics to be found.

#visualization
lda_td <- tidy(lda)

top_terms <- lda_td %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()