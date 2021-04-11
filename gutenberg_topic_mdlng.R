library(tidyverse)
library(tidytext)
library(wordcloud2)
library(forcats)
library(gutenbergr)
library(dplyr)
library(readtext) ## https://cran.r-project.org/web/packages/readtext/vignettes/readtext_vignette.html
library(ggplot2)
library(SnowballC)
library(topicmodels)
library(stm)
library(ldatuning)
library(knitr)
library(LDAvis)

banned_books <- readtext("banned_books/*")

banned_books_tidy <- banned_books %>% 
  rename(book = doc_id) %>%
  na.omit() %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

banned_counts <- banned_books_tidy %>%
  filter(word != "project") %>%
  filter(word != "gutenberg") %>%
  filter(word != "de") %>%
  filter(word != "tis") %>%
  filter(word != "tm") %>%
  filter(word != "chapter") %>%
  count(word, sort = TRUE) 

banned_frequencies <- banned_books_tidy %>%
  filter(word != "project") %>%
  filter(word != "gutenberg") %>%
  filter(word != "de") %>%
  filter(word != "tis") %>%
  filter(word != "tm") %>%
  filter(word != "chapter") %>%
  count(book, word, sort = TRUE) %>%
  group_by(book) %>%
  mutate(proportion = n / sum(n))

banned_dtm <- banned_books_tidy %>%
  count(book, word) %>%
  cast_dtm(book, word, n)

temp <- textProcessor(banned_books$text, 
                      metadata = banned_books,  
                      lowercase=TRUE, 
                      removestopwords=TRUE, 
                      removenumbers=TRUE,  
                      removepunctuation=TRUE, 
                      wordLengths=c(3,Inf),
                      stem=TRUE,
                      onlycharacter= FALSE, 
                      striphtml=TRUE, 
                      customstopwords=NULL)

meta <- temp$meta
vocab <- temp$vocab
docs <- temp$documents

stemmed_books <- banned_books %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word))

stemmed_books

stemmed_dtm <- banned_books %>%
  unnest_tokens(output = word, input = text) %>%
  anti_join(stop_words, by = "word") %>%
  mutate(stem = wordStem(word)) %>%
  count(word, stem, sort = TRUE) %>%
  cast_dtm(word, stem, n)

stemmed_dtm

banned_lda <- LDA(banned_dtm, 
                  k = 20, 
                  control = list(seed = 588)
)

banned_lda

docs <- temp$documents 
meta <- temp$meta 
vocab <- temp$vocab 

banned_stm <- stm(documents=docs, 
                  data=meta,
                  vocab=vocab, 
                  K=20,
                  max.em.its=25,
                  verbose = FALSE)

banned_stm

plot.STM(banned_stm, n = 5)

k_metrics <- FindTopicsNumber(
  banned_dtm,
  topics = seq(10, 75, by = 5),
  metrics = "Griffiths2004",
  method = "Gibbs",
  control = list(),
  mc.cores = NA,
  return_models = FALSE,
  verbose = FALSE,
  libpath = NULL
)

FindTopicsNumber_plot(k_metrics)