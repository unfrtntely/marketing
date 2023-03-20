rm(list=ls(all=TRUE))
library(tm)
library(wordcloud)
library(stringr)
library(dplyr)
library(RColorBrewer)
library(wordcloud2)
library(tidytext)
library(ggplot2)
library(tidyr)

# Upload text
text <- readLines("question1.txt")
# Optional stopwords, were used for the questions to marketing people
my_stopwords <- c("like", "can",'going','marketing',"l’oréal",'marketers','soccer','messi','’s', '’ve','’re',"messi’s","–","“",'football','gatorade','latin', "don’t" , 'able', "also",'get', "know", "now", "lot", "one", "see", "use", "thing",'will','ask','question','big','want')
# Create a corpus from the document
corpus <- Corpus(VectorSource(text))

# Preprocess the corpus
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), my_stopwords))
corpus <- tm_map(corpus, stripWhitespace)

# Create a term-document matrix from the corpus
tdm <- TermDocumentMatrix(corpus)

# Convert the term-document matrix to a matrix
m <- as.matrix(tdm)

# Get the word frequencies and sort them in decreasing order
word_freq <- sort(rowSums(m), decreasing = TRUE)

# Create a data frame of the word frequencies
word_df <- data.frame(word = names(word_freq), freq = word_freq)

# Create the wordcloud 1
wordcloud(words = word_df$word, freq = word_df$freq,  scale=c(1,1),
          min.freq = 2, max.words = 100, random.order = FALSE, 
          rot.per = 0.35, colors = brewer.pal(8, "Dark2"))
# Create another type of wordcloud
wordcloud2(data=word_df, size=0.7, color='random-dark')


#BIGRAMS

# Text to dataframe
text <- data.frame(text = text)
# Optional stopwords
my_stopwords2 <- c('the','is','a',"can",'going','to','soccer','’re',"–","“",'football','gatorade','latin', "don’t" , 'able', "also",'get', "know", "now", "lot",'will')
stop_words <- tidytext::stop_words
# Combine custom stopwords with the default ones
stop_words <- bind_rows(stop_words, tibble(word = my_stopwords2))

# Remove stopwords from text
clean_text <- text %>%
  mutate(text = str_replace_all(text, "\\d+", "")) %>% # remove all digits
  unnest_tokens(word, text) %>%
  anti_join(stop_words)

# Create bigrams
bigrams <- clean_text %>%
  mutate(next_word = lead(word)) %>%
  filter(!is.na(next_word)) %>%
  unite(bigram, word, next_word, sep = " ") %>%
  count(bigram, sort = TRUE)

# Visualize the bigrams
ggplot(bigrams[1:20,], aes(x = reorder(bigram, n), y = n)) +
  geom_bar(stat = "identity", fill = "#4e79a7") +
  coord_flip() +
  labs(title = "Top 20 Bigrams in the Text", title.fontsize = 20,
       x = "Bigram", x.fontsize = 16,
       y = "Frequency", y.fontsize = 16) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 14),
        panel.grid.major.y = element_line(color = "grey80"))
