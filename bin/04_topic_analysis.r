library(scales)
library(topicmodels)
library(tidyverse)
library(tidytext)
library(feather)
library(broom)
library(pander)

# Read in data
text_tidy <- read_feather("./data/text_tidy.feather")
text_tfidf <- read_feather("./data/text_tfidf.feather")
text_raw <- read_feather("./data/text_raw.feather")
genre_tidy <- read_feather("./data/genre_tidy.feather")


# Transform into DocumentTermMatrix form
text_dtm <- text_tidy %>%
  group_by(GameTitle) %>%
  count(word) %>%
  cast_dtm(GameTitle, word, n)


# LDA model for categorization
  # Apply LDA model
  if(FALSE){
    model_lda <- text_dtm %>%
      LDA(k = 4, control = list(seed = 2016))
  } else {
    load("./data/model_lda.RData")    
  }
  
  # Critical terms by topic 
  top_terms <- model_lda %>%
    tidytext:::tidy.LDA() %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)
  
  top_terms %>%
    mutate(term = reorder(term, beta)) %>%
    ggplot(aes(term, beta, fill = factor(topic))) +
    geom_bar(alpha = 0.8, stat = "identity", show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    coord_flip() +
    labs(title = "Critical Terms (Words) by Topic",
         x = "Term (Word)",
         y = "Beta")
  
  # Games by topic
  # -- topic 1: explorative; RPG and Adventure
  # -- topic 2: achievement; strategy amd team sport | social
  # -- topic 3: sensational; race, action, and simulation
  # -- topic 4: social; puzzle, small games | achievement
  games_ldaTop <- model_lda %>%
    tidytext:::tidy.LDA(matrix = "gamma") %>%
    group_by(document) %>%
    top_n(1, wt = gamma) %>%
    ungroup()
  
  ggplot(games_ldaTop, aes(topic)) +
    geom_bar() +
    labs(title = "Topic Distribution",
         x = "Topics",
         y = "Frequency")
  
  pander(arrange(top_n(filter(games_ldaTop, topic == 1), 10, gamma), -gamma))
  pander(arrange(top_n(filter(games_ldaTop, topic == 2), 10, gamma), -gamma))
  pander(arrange(top_n(filter(games_ldaTop, topic == 3), 10, gamma), -gamma))
  pander(arrange(top_n(filter(games_ldaTop, topic == 4), 10, gamma), -gamma))
  
  # Games with topic scores for later analysis
  games_lda <- model_lda %>%
    tidytext:::tidy.LDA(matrix = "gamma") %>%
    spread(topic, gamma) %>%
    left_join(text_raw, by = c("document" = "GameTitle"))
  write_feather(games_lda, "./data/games_lda.feather")
  
  