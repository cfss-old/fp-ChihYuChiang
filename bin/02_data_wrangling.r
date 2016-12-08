library(tidytext)
library(tidyverse)
library(SnowballC)
library(feather)

# Load stop word dictionery
data(stop_words)


# Main dataset
  # Read data, adjust encoding, remove duplicate data
  text_raw <- read_csv("./data/df_cb_main.csv") %>%
    mutate(Review = iconv(Review, "ASCII", "UTF-8")) %>%
    transmute(GameTitle = `Game Title`, Review, GSScore = `GS Score`, ESRB, ReleaseDate = `Release Date`) %>%
    arrange(GameTitle, GSScore) %>%
    distinct(GameTitle, .keep_all = TRUE)
  
  # Remove game title words in each review
  title_tidy <- text_raw %>%
    mutate(TitleToken = GameTitle) %>%
    unnest_tokens(word, TitleToken) %>%
    select(GameTitle, word)
  i <- 1
  for(title in text_raw$GameTitle){
    title_tidy_single <- filter(title_tidy, GameTitle == text_raw[[1]][i])
    for(titleText in title_tidy_single$word){
      filterTextS = paste("(^(", titleText, ") )", sep = "")
      filterTextO = paste("(( ", titleText, "))", sep = "")
      text_raw[i, 2] <- gsub(filterTextS, "", text_raw[i, 2], ignore.case = TRUE)      
      text_raw[i, 2] <- gsub(filterTextO, "", text_raw[i, 2], ignore.case = TRUE)   
    }
    i <- i + 1
  }

  # Tokenize review, remove stop words, stem words
  text_tidy <- text_raw %>%
    unnest_tokens(word, Review) %>%
    anti_join(stop_words, by = "word") %>%
    mutate(word = wordStem(word))
  
  # Remove frequent general topic words
  wordSieve = c("game", "games", "game's", "play", "feel", "time", "experience", "makes")
  text_tidy <- text_tidy %>%
    filter(!word %in% wordSieve)
  
  # Generate tfidf index
  text_tfidf <- text_tidy %>%
    group_by(GameTitle) %>%
    count(word, sort = TRUE) %>%
    bind_tf_idf(word, GameTitle, n)
  
  # Save in feather file
  write_feather(text_raw, "./data/text_raw.feather")  
  write_feather(text_tidy, "./data/text_tidy.feather")
  write_feather(text_tfidf, "./data/text_tfidf.feather")
  
  
# Genre dataset
  # Read data, remove duplicate data'
  genre_tidy <- read_csv("./data/df_cb_genre.csv") %>%
    transmute(GameTitle = `Game Title`, Genre) %>%
    distinct(GameTitle, Genre, .keep_all = TRUE)
  
  # Save in feather file
  write_feather(genre_tidy, "./data/genre_tidy.feather")
