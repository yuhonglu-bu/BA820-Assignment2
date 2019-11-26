# Topic 1: Explore sentiment analysis to the dataset.  Are there any interesting patterns.  Describe/visualize your findings.
# Topic 2:  Explore a topic model to the reviews.  For the k that you select, summarize your findings.
# Topic 3:  Summarize your work and your recommendation as to where the organization should prioritize their efforts.

library(tidyverse)
library(rvest)
library(tidytext)
library(wordcloud)
library(quanteda)
library(factoextra)
library(sentimentr)
library(cluster)
library(topicmodels)

setwd("~/Documents/BU MSBA /BA820 USML/Assignment/Assignment2")

# word cloud plot
data <- readRDS('hw2.rds')
skimr::skim(data)
data.tidy <- data %>% 
  unnest_tokens(token, review_text)
stopwords::stopwords_getsources()
stopwords::stopwords_getlanguages("snowball")
stopwords::stopwords_getlanguages("stopwords-iso")
stopwords::stopwords_getlanguages("smart")
data.tidy2 <- data.tidy %>% 
  anti_join(get_stopwords(), by = c('token' = 'word'))
data.sum <- data.tidy2 %>% 
  group_by(token) %>% 
  count(sort = T)
wordcloud(words = data.sum$token, 
          freq = data.sum$n, 
          min.freq = 10, 
          max.words = 50)
data.improve <- data.tidy2 %>% 
  group_by(department_name, token) %>% 
  count(sort = T) %>% 
  filter(department_name == 3 | department_name == 6)
wordcloud(words = data.improve$token, 
          freq = data.improve$n, 
          min.freq = 10, 
          max.words = 50)
data.improve.division <- data.tidy2 %>% 
  group_by(division_name, token) %>% 
  count(sort = T) %>% 
  filter(division_name == 3)
wordcloud(words = data.improve.division$token, 
          freq = data.improve.division$n, 
          min.freq = 10, 
          max.words = 50)
# AFINN
data.sent1 <- data.tidy2 %>% 
  inner_join(get_sentiments('afinn'),
             by = c('token' = 'word')) %>% 
  group_by(product_id) %>% 
  summarise(affin_polarity = sum(value)) %>% 
  inner_join(data)
ggplot(data.sent1, aes(x = factor(division_name), y = affin_polarity)) + 
  geom_boxplot() + 
  labs(title="AFINN Sentiment of Division Review")
ggplot(data.sent1, aes(x = factor(department_name), y = affin_polarity)) + 
  geom_boxplot() + 
  labs(title="AFINN Sentiment of Department Review")
ggplot(data.sent1, aes(x = factor(class_name), y = affin_polarity)) + 
  geom_boxplot() + 
  labs(title="AFINN Sentiment of Class Review")

# Average sentiment value
data.review <- data$review_text
data <- data %>% 
  mutate(review_index = row_number())
data.sentence <- get_sentences(data.review)
data.sent2 <- sentiment(data.sentence)
data.sent2.avg <- data.sent2 %>% 
  group_by(element_id) %>% 
  summarise(polarity = mean(sentiment))

product_sent <- with(data, sentiment_by(get_sentences(review_text),
                                        list(division_name, review_index)))
ggplot(product_sent, aes(x = review_index, y = ave_sentiment)) +
  geom_line(aes(col = division_name)) +
  geom_smooth(method = 'loess') +
  facet_grid(rows = vars(division_name)) +
  labs(title="Average Sentiment of Division Review")

department_sent <- with(data, sentiment_by(get_sentences(review_text),
                                        list(department_name, review_index)))
ggplot(department_sent, aes(x = review_index, y = ave_sentiment)) +
  geom_line(aes(col = department_name)) +
  geom_smooth(method = 'loess') +
  facet_grid(rows = vars(department_name)) +
  labs(title="Average Sentiment of Department Review")

class_sent <- with(data, sentiment_by(get_sentences(review_text),
                                           list(class_name, review_index)))
ggplot(class_sent, aes(x = review_index, y = ave_sentiment)) +
  geom_line(aes(col = class_name)) +
  geom_smooth(method = 'loess') +
  facet_grid(rows = vars(class_name)) +
  labs(title="Average Sentiment of Class Review")

# Topic Model
data.corpus <- corpus(data$review_text)
summary(data.corpus, n = 20, showmeta = T)
data.dfm <- dfm(data.corpus,
               remove_punct= T,
               remove = stopwords(),
               remove_numbers= T,
               remove_symbols= T) %>% 
  dfm_trim(min_termfreq = 2,
           max_docfreq = .5,
           docfreq_type = "prop")
data.dtm <- convert(data.dfm, 'topicmodels')
data.lda <- LDA(data.dtm, k = 2, control = list(seed = 729))
terms(data.lda, 20)
