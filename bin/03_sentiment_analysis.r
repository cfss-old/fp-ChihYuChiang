library(tidytext)
library(tidyverse)
library(sentimentr)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)
library(reshape2)
library(knitr)
library(broom)
library(pander)
library(modelr)
library(caret)
library(feather)

#inport data
genre_tidy <- read_feather("./data/genre_tidy.feather")
text_tidy <- read_feather("./data/text_tidy.feather")
text_tfidf <- read_feather("./data/text_tfidf.feather")
text_raw <- read_feather("./data/text_raw.feather")
games_lda <- read_feather("./data/games_lda.feather")

## Word Frequency in the reviews
### General Word
text_count <- text_tidy %>%
  count(word, sort = TRUE) %>%
  with(wordcloud(word, n, max.words = 200, colors = brewer.pal(8, "Dark2"), random.order = FALSE))

text_count <- text_tidy %>%
  count(word, sort = TRUE) %>%
  arrange(desc(n)) %>%
  head(20) %>%
  ggplot(aes(word,n))+
  geom_bar(stat = "identity")+
  ggtitle("The most frequent word in game review-top20")+
  ylab ("amount")+
  xlab ("word")+
  coord_flip()

### Word frequency by Tfidf
ggplot(text_tfidf[1 : 20,], aes(word, tf_idf)) +
  geom_bar(alpha = 0.8, stat = "identity") +
  labs(title = "Highest tf-idf words in All Game Reviews",
       x = NULL, y = "tf-idf") +
  coord_flip()

### Sentiment Word
nrc <- get_sentiments("nrc") %>%
mutate(word = wordStem(word)) %>%
distinct()

bing <-  get_sentiments("bing") %>%
mutate(word = wordStem(word)) %>%
distinct()

# Sentiment words by frequency
text_sentCount <- text_tidy %>%
inner_join(nrc, by = "word") %>%
group_by(sentiment) %>%
count(word)

#get the plot about the sentiment word most frequently used in game reviews
bing_word_counts <- text_tidy %>%
inner_join(bing, by = "word") %>%
count(word, sentiment, sort = TRUE) %>%
ungroup()

bing_word_counts %>%
filter(n > 1800) %>%
mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
mutate(word = reorder(word, n)) %>%
ggplot(aes(word, n, fill = sentiment)) +
geom_bar(alpha = 0.8, stat = "identity") +
labs(y = "Contribution to sentiment",
x = NULL) +
coord_flip()

# Positive emotional words by frequency
text_sentCountPo <- text_tidy %>%
count(word, sort = TRUE) %>%
inner_join(nrc, by = "word") %>%
filter(sentiment %in% c("anticipation", "joy", "surprise", "trust")) %>%
acast(word ~ sentiment, value.var = "n", fill = 0) %>%
comparison.cloud(title.size = 1.5, max.words = 200)

# Negative emotional words by frequency
text_sentCountNe <- text_tidy %>%
count(word, sort = TRUE) %>%
inner_join(nrc, by = "word") %>%
filter(sentiment %in% c("anger", "disgust", "fear", "sadness")) %>%
acast(word ~ sentiment, value.var = "n", fill = 0)%>%
comparison.cloud(title.size = 1.5, max.words = 200)

### Sentiment Intensity
text_sent <- text_tidy %>%
inner_join(nrc, by = "word") %>%
filter(sentiment != "positive", sentiment != "negative")

reorder_bysize <- function(x) {
factor(x, levels = names(sort(table(x), decreasing = TRUE)))
}

ggplot(text_sent, aes(reorder_bysize(sentiment))) +
geom_bar(fill = rep(c("salmon", "skyblue"), 4)) +
labs(title = "Sentiment Intensity (by sentiment word count)",
x = "Sentment",
y = "Sentiment word count")

text_sentTfidf1 <- text_tfidf %>%
group_by(word) %>%
summarize(tfidf_avg = mean(tf_idf)) %>%
right_join(text_sentCount, by = "word")

top_n(filter(text_sentTfidf1, sentiment == "disgust"), 5, tfidf_avg) %>% 
arrange(desc(tfidf_avg)) %>% kable()

top_n(filter(text_sentTfidf1, sentiment == "joy"), 5, tfidf_avg) %>% 
arrange(desc(tfidf_avg)) %>% kable()

## Sentiment Words and General Evaluation Score
### General Description
text_tidy_level <- text_tidy %>%
mutate(level = ifelse(GSScore > 8.5, "high",
ifelse(GSScore < 5, "low", "medium"))) %>%
filter(is.na(level) == F )

level_count <- text_tidy_level %>%
count(level)

text_sent_nrc <- text_tidy_level %>%
inner_join(nrc, by = "word") %>%
count(level, sentiment)

bing <- get_sentiments("bing") %>%
mutate(word = wordStem(word)) %>%
distinct()

text_sent_PN <- text_tidy_level %>%
inner_join(bing, by = "word") %>%
count(level, sentiment)

text_sent_PN %>%
ggplot()+
geom_bar(mapping = aes(x = level, y =n, fill = sentiment),stat = "identity", position = "fill")+
ggtitle("The positive-negative words proportion in game reviews")+
ylab ("percentage")+
xlab ("The level of game score")

text_sent_nrc_analysis <- text_sent_nrc %>%
inner_join(level_count, by = "level") %>%
mutate(percentage = n.x / n.y)

text_sent_nrc_analysis %>%
filter((sentiment != "negative") & (sentiment != "positive"))%>%
ggplot(aes(sentiment, percentage))+
geom_bar(aes(fill = sentiment), stat = "identity")+
facet_wrap(~ level, nrow = 2)+
ggtitle("The different types of sentiment words proportion in game reviews")+
coord_polar()

text_sent_nrc %>%
filter((sentiment != "negative") & (sentiment != "positive"))%>%
ggplot()+
geom_bar(mapping = aes(x = level, y =n, fill = sentiment),stat = "identity", position = "fill")+
ggtitle("The different types of sentiment words proportion in game reviews")+
ylab ("percentage")+
xlab ("The level of game score")

### Explore the relationship
# Calculate sentiment tf_idfscore by game, 
text_sentTfidf <- text_tfidf %>%
left_join(text_sentCount, by = "word") %>%
group_by(GameTitle, sentiment) %>%
summarize(tfidfScore = sum(tf_idf)) %>%
spread(sentiment, tfidfScore) %>%
left_join(text_raw, by = "GameTitle") %>%
select(-`<NA>`, -Review) %>%
na.omit()

# save for later
write_feather(text_sentTfidf, "./data/text_sentTfidf.feather")

# Exploratory
text_sentTfidf_plt <- text_sentTfidf %>%
gather(sentiment, tfidf, anger : trust)

ggplot(filter(text_sentTfidf_plt, sentiment %in% c("positive", "negative")),
aes(tfidf, GSScore)) + 
geom_jitter(height = 0.1, width = 0.1, alpha = 0.2) +
geom_smooth() +
facet_wrap(~ sentiment)

ggplot(filter(text_sentTfidf_plt, sentiment %in% c("anger", "disgust", "fear", "sadness")),
aes(tfidf, GSScore)) + 
geom_jitter(height = 0.1, width = 0.1, alpha = 0.2) +
geom_smooth() +
facet_wrap(~ sentiment)

ggplot(filter(text_sentTfidf_plt, sentiment %in% c("anticipation", "joy", "surprise", "trust")),
aes(tfidf, GSScore)) + 
geom_jitter(height = 0.1, width = 0.1, alpha = 0.2) +
geom_smooth() +
facet_wrap(~ sentiment)


## Model Building
text_sentTfidf <- text_sentTfidf %>%
mutate(GSScore_cat1 = cut(GSScore,
c(0, (mean(text_sentTfidf$GSScore) - sd(text_sentTfidf$GSScore)), (mean(text_sentTfidf$GSScore) + sd(text_sentTfidf$GSScore)), 10),
labels = c("Low", "Medium", "High")),
GSScore_cat2 = cut(GSScore,
c(0, mean(text_sentTfidf$GSScore), 10),
labels = c("Low", "High")))

# Divide training and testing groups
inTraining <- resample_partition(text_sentTfidf, c(testing = 0.3, training = 0.7))
training <- inTraining$training %>% tbl_df()
testing <- inTraining$testing %>% tbl_df()

# 5-fold cross validation, repeat 10 times
fitControl <- trainControl(
method = "repeatedcv",
number = 5,
repeats = 10)

###Linear Model
# Linear regression model
lm_fit1 <- train(GSScore ~ positive + negative, data = training, method = "lm", trControl = fitControl)
lm_fit2 <- train(GSScore ~ anticipation + trust + surprise + joy + sadness + anger + fear + disgust, data = training, method = "lm", trControl = fitControl)
pander(summary(lm_fit1))
pander(summary(lm_fit2))

lm_pred <- predict(lm_fit2, testing)
pander(postResample(pred = lm_pred, obs = testing$GSScore))

## General additive model
# General additive model
gam_fit1 <- train(GSScore ~ positive + negative, data = training, method = 'gamLoess', trControl = fitControl)
gam_fit2 <- train(GSScore ~ anticipation + trust + surprise + joy + sadness + anger + fear + disgust, data = training, method = 'gamLoess', trControl = fitControl)
pander(summary(gam_fit1)[4])
pander(summary(gam_fit2)[4])

gam_pred <- predict(gam_fit2, testing)
pander(postResample(pred = gam_pred, obs = testing$GSScore))
