library(tidyverse)
library(tidytext)
# devtools::install_github("dgrtwo/drlib")
library(drlib)

# tokenize and afinn sentiment the little mix albums
littlemix_uni_sent <- littlemix_lyrics %>% 
  unnest_tokens(word, lyric) %>% 
  left_join(get_sentiments("afinn")) %>%
  mutate(score = ifelse(is.na(score), 0, score)) %>%
  left_join(littlemix_albums_df)


# what are the most used sentiment words
most_sentimented <- littlemix_uni_sent %>% 
  group_by(word) %>% 
  summarise(sentiment_sum = sum(score)) %>% 
  ungroup() %>% 
  arrange(desc(sentiment_sum))

# and album splits
most_sentimented_album <- littlemix_uni_sent %>%
  group_by(album, word) %>%
  summarise(sentiment_sum = sum(score)) %>% 
  arrange(desc(sentiment_sum))

# get top and bottom 10
top_bottom <- bind_rows(top_n(most_sentimented, 10),
                        top_n(most_sentimented, -10)) %>%
  mutate(position = row_number(),
         sign = sign(sentiment_sum))

# get top and bottom 5 split by album
top_bottom_album <- bind_rows(top_n(most_sentimented_album, 5),
                              top_n(most_sentimented_album, -5)) %>%
  left_join(littlemix_albums_df) %>%
  mutate(position = row_number(),
         sign = sign(sentiment_sum))

# lets make a chart over the entire Little mix corpus
top_bottom %>%
  ggplot(aes(fct_reorder(word, -position), sentiment_sum)) + 
  geom_col(aes(fill = factor(sign))) +
  coord_flip() +
  scale_fill_manual(values = c("tomato3", "palegreen3")) +
  theme(legend.position = "none") +
  labs(title = 'Most "sentimented" words across all Little Mix albums',
       subtitle = "Using the AFINN sentiment dictionary",
       x = "Total sum of sentiment contribution",
       y = "Word")

# and top/bottom 5 words per album
top_bottom_album %>%
  ggplot(aes(reorder_within(word, -position, album), sentiment_sum)) + 
  geom_col(aes(fill = factor(sign))) +
  coord_flip() +
  scale_fill_manual(values = c("tomato3", "palegreen3")) +
  scale_x_reordered() + 
  facet_wrap(~ fct_reorder(album, album_no), scales = "free") +
  theme(legend.position = "none") +
  labs(title = 'Most "sentimented" words across all Little Mix albums',
       subtitle = "Using the AFINN sentiment dictionary",
       y = "Total sum of sentiment contribution",
       x = "Word")

# create a df with a rolling sum of senitment through the album
littlemix_rolling_sent <- littlemix_lyrics %>% 
  unnest_tokens(word, lyric) %>% 
  left_join(get_sentiments("afinn")) %>%
  mutate(score = ifelse(is.na(score), 0, score)) %>%
  left_join(littlemix_albums_df) %>%
  group_by(album) %>%
  mutate(pos = row_number()) %>% 
  mutate(rollsum = cumsum(score))

# initial visual on total album sentiment
littlemix_rolling_sent %>%
  group_by(album_no, album) %>%
  summarise(album_sentiment = sum(score)) %>%
  ggplot(aes(fct_reorder(album, album_no), album_sentiment)) +
  geom_col() +
  ggtitle("Little Mix Sentiment per Album") +
  xlab("Album name ordered by release date") +
  ylab("Cumulative Sentiment per Album") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# wow, they're a bit super positive, apart from DNA
# let's look at how the sentiment builds through the albums
littlemix_rolling_sent %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ fct_reorder(album, album_no), scales = "free_x") +
  ggtitle("Little Mix Rolling and Cumulative Sentiment per Album") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album")

# they're pretty relentlessly positive, especially Glory Days.
# let's look at total sentiment for all songs and order
all_songs <- littlemix_rolling_sent %>%
  group_by(album_no, album, track_n, track_title) %>%
  summarise(total_sentiment = sum(score)) %>%
  arrange(desc(total_sentiment))

# let's look at the top and bottom
head(all_songs)
tail(all_songs)

# let's look at your love
littlemix_rolling_sent %>% 
  filter(track_title == "Your Love") %>%
  group_by(word) %>%
  summarise(n = n(),
            total_sent = sum(score)) %>%
  arrange(desc(total_sent))

# loves a big scorer at 3 points, so saying it 78 times (clues in the name of the track)
# means a ridiculous boost
# let's look at the bottom end
littlemix_rolling_sent %>% 
  filter(track_title == "No More Sad Songs") %>%
  group_by(word) %>%
  summarise(n = n(),
            total_sent = sum(score)) %>%
  arrange(total_sent)

# yep, clue's in the name again, if you say sad (-2) a lot it'll add up