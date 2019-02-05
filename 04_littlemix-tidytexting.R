library(tidyverse)
library(tidytext)

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

# get top and bottom 10
top_bottom_album <- bind_rows(top_n(most_sentimented_album, 5),
                              top_n(most_sentimented_album, -5)) %>%
  left_join(littlemix_albums_df) %>%
  mutate(position = row_number(),
         sign = sign(sentiment_sum))

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



# need to rebuild the rolling s
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

# tres interesting, definitely more negs than pos
# let's look at how the sentiment builds through the albums
littlemix_rolling_sent %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ fct_reorder(album, album_no), scales = "free_x") +
  ggtitle("Little Mix Rolling and Cumulative Sentiment per Album") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album")

# some odd stuff going on with the bends, kid a and hail to the thief
# let's look at total sentiment for all songs and order
all_songs <- littlemix_rolling_sent %>%
  group_by(album_no, album, track_n, track_title) %>%
  summarise(total_sentiment = sum(score)) %>%
  arrange(desc(total_sentiment))

head(all_songs)
tail(all_songs)

# let's look at nice dream
littlemix_rolling_sent %>% 
  filter(track_title == "Your Love") %>%
  group_by(word) %>%
  summarise(n = n(),
            total_sent = sum(score)) %>%
  arrange(desc(total_sent))

# by using the lyric "nice dream" 17 times it adds 68 to total sentiment for the song
# seems the clue is in the track title!
# is it a similar story with "optimistic" in kid a?
littlemix_rolling_sent %>% 
  filter(track_title == "How Ya Doin?") %>%
  group_by(word) %>%
  summarise(n = n(),
            total_sent = sum(score)) %>%
  arrange(total_sent)

# yes broadly, using best 17 times and good 5 gave the song 66
# both very optimisitic words!
# so for each of these outliers we have repeated "good" sentiment words being used
# let's look at the bottom two
radiohead_rolling_sent %>% 
  filter(track_title == "A Punchup at a Wedding" | track_title == "A Wolf at the Door") %>%
  group_by(track_title, word) %>%
  summarise(n = n(),
            total_sent = sum(score)) %>%
  arrange(total_sent)

# interestingly both tracks are driven by repeated instances of the word "no"

# let's have a look at hail to the thief visually
littlemix_rolling_sent %>% 
  filter(album == "dna") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ fct_reorder(track_title, track_n), scales = "free") +
  ggtitle("DNA Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album")

