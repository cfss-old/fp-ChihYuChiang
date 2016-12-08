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

nrc <- get_sentiments("nrc") %>%
  mutate(word = wordStem(word)) %>%
  distinct()

# Sentiment words by frequency
text_sentCount <- text_tidy %>%
  inner_join(nrc, by = "word") %>%
  group_by(sentiment) %>%
  count(word)

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
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(title.size = 1.5, max.words = 200)

# Tf_idf
text_tfidf <- text_tidy %>%
  group_by(GameTitle) %>%
  count(word, sort = TRUE) %>%
  bind_tf_idf(word, GameTitle, n)

# Sentiment words by tf_idf
text_sentTfidf <- text_tfidf %>%
  group_by(word) %>%
  summarize(tfidf_avg = mean(tf_idf)) %>%
  right_join(text_sentCount, by = "word")

top_n(filter(text_sentTfidf, sentiment == "positive"), 5, tfidf_avg) %>% 
  arrange(desc(tfidf_avg)) %>% kable()
top_n(filter(text_sentTfidf, sentiment == "negative"), 5, tfidf_avg) %>% 
  arrange(desc(tfidf_avg)) %>% kable()

top_n(filter(text_sentTfidf, sentiment == "anger"), 5, tfidf_avg) %>% 
  arrange(desc(tfidf_avg)) %>% kable()
top_n(filter(text_sentTfidf, sentiment == "disgust"), 5, tfidf_avg) %>% 
  arrange(desc(tfidf_avg)) %>% kable()
top_n(filter(text_sentTfidf, sentiment == "fear"), 5, tfidf_avg) %>% 
  arrange(desc(tfidf_avg)) %>% kable()
top_n(filter(text_sentTfidf, sentiment == "sadness"), 5, tfidf_avg) %>% 
  arrange(desc(tfidf_avg)) %>% kable()

top_n(filter(text_sentTfidf, sentiment == "anticipation"), 5, tfidf_avg) %>% 
  arrange(desc(tfidf_avg)) %>% kable()
top_n(filter(text_sentTfidf, sentiment == "joy"), 5, tfidf_avg) %>% 
  arrange(desc(tfidf_avg)) %>% kable()
top_n(filter(text_sentTfidf, sentiment == "surprise"), 5, tfidf_avg) %>% 
  arrange(desc(tfidf_avg)) %>% kable()
top_n(filter(text_sentTfidf, sentiment == "trust"), 5, tfidf_avg) %>% 
  arrange(desc(tfidf_avg)) %>% kable()

# Calculate sentiment tf_idfscore by game, 
text_sentTfidf <- text_tfidf %>%
  left_join(text_sentCount, by = "word") %>%
  group_by(GameTitle, sentiment) %>%
  summarize(tfidfScore = sum(tf_idf)) %>%
  spread(sentiment, tfidfScore) %>%
  left_join(data_raw, by = "GameTitle") %>%
  select(-`<NA>`, -Review) %>%
  na.omit()

# Exploratory
text_sentTfidf_plt <- text_sentTfidf %>%
  gather(sentiment, tfidf, anger : trust)

ggplot(filter(text_sentTfidf_plt, sentiment %in% c("positive", "negative")),
       aes(tfidf, GSScore)) + 
  geom_jitter(height = 0.1, width = 0.1, alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~ sentiment)

ggplot(text_sentTfidf, aes(log(positive/negative), GSScore)) + 
  geom_jitter(height = 0.05, width = 0.05, alpha = 0.2) +
  geom_smooth()

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

# Prepare for model fitting
ggplot(text_sentTfidf, aes(GSScore)) +
  geom_bar()

text_sentTfidf <- text_sentTfidf %>%
  mutate(GSScore_cat1 = cut(GSScore,
                            c(0, (mean(text_sentTfidf$GSScore) - sd(text_sentTfidf$GSScore)), (mean(text_sentTfidf$GSScore) + sd(text_sentTfidf$GSScore)), 10),
                            labels = c("Low", "Medium", "High")),
         GSScore_cat2 = cut(GSScore,
                            c(0, mean(text_sentTfidf$GSScore), 10),
                            labels = c("Low", "High")))

inTraining <- resample_partition(text_sentTfidf, c(testing = 0.3, training = 0.7))
training <- inTraining$training %>% tbl_df()
testing <- inTraining$testing %>% tbl_df()

fitControl <- trainControl(## 5-fold CV, repeat 10 times
  method = "repeatedcv",
  number = 5,
  repeats = 10)

# Linear regression model
lm_fit <- train(GSScore ~ positive + negative, data = training, method = "lm", trControl = fitControl)
lm_fit <- train(GSScore ~ positive/negative, data = training, method = "lm", trControl = fitControl)
lm_fit <- train(GSScore ~ anticipation + trust + surprise + joy + sadness + anger + fear + disgust, data = training, method = "lm", trControl = fitControl)
summary(lm_fit)
lm_fit

lm_pred <- predict(lm_fit, testing)
postResample(pred = lm_pred, obs = testing$GSScore)

# General additive model
gam_fit <- train(GSScore ~ positive + negative, data = training, method = 'gamLoess', trControl = fitControl)
gam_fit <- train(GSScore ~ anticipation + trust + surprise + joy + sadness + anger + fear + disgust, data = training, method = 'gamLoess', trControl = fitControl)
summary(gam_fit)
gam_fit

gam_pred <- predict(gam_fit, testing)
postResample(pred = gam_pred, obs = testing$GSScore)

# Random forest model
rf_fit <- train(GSScore_cat1 ~ positive + negative, data = training, method = 'rf', ntree = 200, trControl = fitControl)
rf_fit <- train(GSScore_cat1 ~ anticipation + trust + surprise + joy + sadness + anger + fear + disgust, data = training, method = 'rf', ntree = 200, trControl = fitControl)
rf_fit <- train(GSScore_cat2 ~ positive + negative, data = training, method = 'rf', ntree = 200, trControl = fitControl)
rf_fit <- train(GSScore_cat2 ~ anticipation + trust + surprise + joy + sadness + anger + fear + disgust, data = training, method = 'rf', ntree = 200, trControl = fitControl)
rf_fit$finalModel
varImpPlot(rf_fit$finalModel)
kable(rf_fit$finalModel$confusion)

rf_pred <- predict(rf_fit, testing)
postResample(pred = rf_pred, obs = testing$GSScore_cat)