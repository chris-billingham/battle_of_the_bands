library(tidyverse)
library(tidytext)

littlemix_uni_sent <- littlemix_lyrics %>% 
  unnest_tokens(word, lyric) %>% 
  left_join(get_sentiments("afinn")) %>%
  mutate(score = ifelse(is.na(score), 0, score)) %>%
  left_join(littlemix_albums) %>%
  group_by(album) %>%
  mutate(pos = row_number()) %>% 
  mutate(rollsum = cumsum(score))

# initial visual on total album sentiment
littlemix_uni_sent %>%
  group_by(album_no, album) %>%
  summarise(album_sentiment = sum(score)) %>%
  ggplot(aes(fct_reorder(album, album_no), album_sentiment)) +
  geom_col() +
  ggtitle("Little Mix Sentiment per Album") +
  xlab("Album name ordered by release date") +
  ylab("Cumulative Sentiment per Album") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# let's look at how the sentiment builds through the albums
littlemix_uni_sent %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ fct_reorder(album, album_no), scales = "free") +
  ggtitle("Little Mix Cumulative Sentiment per Album") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album")

# dna is definitely the off one out there
littlemix_uni_sent %>% 
  filter(album == "dna") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ fct_reorder(track_title, track_n), scales = "free_x") +
  ggtitle("DNA Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album")
