
library(tidyverse)
library(tidytext)
# devtools::install_github("dgrtwo/drlib")
library(drlib)

# get the afinn sentiment for each one
radiohead_uni_sent <- radiohead_lyrics %>% 
  unnest_tokens(word, lyric) %>% 
  left_join(get_sentiments("afinn")) %>%
  mutate(score = ifelse(is.na(score), 0, score)) %>%
  left_join(radiohead_albums_df)

# what are the most used sentiment words
most_sentimented <- radiohead_uni_sent %>% 
  group_by(word) %>% 
  summarise(sentiment_sum = sum(score)) %>% 
  ungroup() %>% 
  arrange(desc(sentiment_sum))

# and album splits
most_sentimented_album <- radiohead_uni_sent %>%
  group_by(album, word) %>%
  summarise(sentiment_sum = sum(score)) %>% 
  arrange(desc(sentiment_sum))

# get top and bottom 10
top_bottom <- bind_rows(top_n(most_sentimented, 10),
                        top_n(most_sentimented, -10)) %>%
  mutate(position = row_number(),
         sign = sign(sentiment_sum))

# album top-bottom
# get top and bottom 5 per album
top_bottom_album <- bind_rows(top_n(most_sentimented_album, 5),
                              top_n(most_sentimented_album, -5)) %>%
  left_join(radiohead_albums_df) %>%
  mutate(position = row_number(),
         sign = sign(sentiment_sum))

# let's chart the overall lyric corpus
top_bottom %>%
  ggplot(aes(fct_reorder(word, -position), sentiment_sum)) + 
  geom_col(aes(fill = factor(sign))) +
  coord_flip() +
  scale_fill_manual(values = c("tomato3", "palegreen3")) +
  theme(legend.position = "none") +
  labs(title = 'Most "sentimented" words across all Radiohead albums',
       subtitle = "Using the AFINN sentiment dictionary",
       x = "Total sum of sentiment contribution",
       y = "Word")

# and let's chart top/bottom 5 per album (note use of reorder_within from drlib)
top_bottom_album %>%
  ggplot(aes(reorder_within(word, -position, album), sentiment_sum)) + 
  geom_col(aes(fill = factor(sign))) +
  coord_flip() +
  scale_fill_manual(values = c("tomato3", "palegreen3")) +
  scale_x_reordered() + 
  facet_wrap(~ fct_reorder(album, album_no), scales = "free") +
  theme(legend.position = "none") +
  labs(title = 'Most "sentimented" words across all Radiohead albums',
       subtitle = "Using the AFINN sentiment dictionary",
       y = "Total sum of sentiment contribution",
       x = "Word")

# create a df which looks at some kind of rolling sentiment through an album
radiohead_rolling_sent <- radiohead_lyrics %>% 
  unnest_tokens(word, lyric) %>% 
  left_join(get_sentiments("afinn")) %>%
  mutate(score = ifelse(is.na(score), 0, score)) %>%
  left_join(radiohead_albums_df) %>%
  group_by(album) %>%
  mutate(pos = row_number()) %>% 
  mutate(rollsum = cumsum(score))

# initial visual on total album sentiment
radiohead_rolling_sent %>%
  group_by(album_no, album) %>%
  summarise(album_sentiment = sum(score)) %>%
  ggplot(aes(fct_reorder(album, album_no), album_sentiment)) +
  geom_col() +
  ggtitle("Radiohead Sentiment per Album") +
  xlab("Album name ordered by release date") +
  ylab("Cumulative Sentiment per Album") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# tres interesting, definitely more negs than pos
# let's look at how the sentiment builds through the albums
radiohead_rolling_sent %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ fct_reorder(album, album_no), scales = "free_x") +
  ggtitle("Radiohead Rolling and Cumulative Sentiment per Album") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album")

# some odd stuff going on with the bends, kid a and hail to the thief
# let's look at total sentiment for all songs and order
all_songs <- radiohead_rolling_sent %>%
  group_by(album_no, album, track_n, track_title) %>%
  summarise(total_sentiment = sum(score)) %>%
  arrange(desc(total_sentiment))

# what we got at the tp and bottom
head(all_songs)
tail(all_songs)

# let's look at nice dream
radiohead_rolling_sent %>% 
  filter(track_title == "(Nice Dream)") %>%
  group_by(word) %>%
  summarise(n = n(),
            total_sent = sum(score)) %>%
  arrange(desc(total_sent))

# by using the lyric "nice dream" 17 times it adds 68 to total sentiment for the song
# seems the clue is in the track title!
# is it a similar story with "optimistic" in kid a?
radiohead_rolling_sent %>% 
  filter(track_title == "Optimistic") %>%
  group_by(word) %>%
  summarise(n = n(),
            total_sent = sum(score)) %>%
  arrange(desc(total_sent))

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
# checking the lyrics on azlyrics, punchup starts with "no x 43", hahahaha

# let's have a look at hail to the thief visually
radiohead_rolling_sent %>% 
  filter(album == "hail to the thief") %>%
  ggplot(aes(pos, rollsum)) + 
  geom_line() + 
  facet_wrap(~ fct_reorder(track_title, track_n), scales = "free_x") +
  ggtitle("Hail to the Thief Cumulative Sentiment per Song") +
  xlab("Word Position in Album") +
  ylab("Cumulative Sentiment per Album")

# so actually you could argue hail to the thief is pretty neutral
# but those two tracks really bring the sentiment down
# let's play a game, let's stop the repeated lyric "trick"
# what happens if we remove the top 2 and bottom 2 to overall radiohead 
# sentiments across the albums
radiohead_rolling_sent %>%
  filter(track_title != "(Nice Dream)" & track_title != "Optimistic" & 
           track_title != "A Punchup at a Wedding" & track_title != "A Wolf at the Door") %>%
  group_by(album_no, album) %>%
  summarise(album_sentiment = sum(score)) %>%
  ggplot(aes(fct_reorder(album, album_no), album_sentiment)) +
  geom_col() +
  ggtitle("Radiohead Sentiment per Album",
          subtitle = "Minus top 2 and bottom 2 sentiment tracks") +
  xlab("Album name ordered by release date") +
  ylab("Cumulative Sentiment per Album") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
