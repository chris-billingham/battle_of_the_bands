library(tidyverse)
library(tidyr)
library(scales)

# we need the two uni_sent dfs we made in the first two codes
# let's add the actual bands in
littlemix_uni_sent$band <- "little_mix"
radiohead_uni_sent$band <- "radiohead"

# push it together, remove stop words
# also in a lot of lyrics for Little Mix it calls out who's singing
# so we have to remove the girl's names from the list
# sorry ladies
both_bands <- bind_rows(littlemix_uni_sent, radiohead_uni_sent) %>%
  anti_join(stop_words) %>%
  filter(!(word %in% c("jesy", "leigh", "anne", "jade", "perrie")))

# let's look at the frequency on a per band basis
frequency <- both_bands %>% 
  group_by(band) %>% 
  count(word, sort = TRUE) %>% 
  left_join(both_bands %>% 
              group_by(band) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

# add in the sentiment
frequency <- frequency %>% 
  select(band, word, freq) %>% 
  spread(band, freq) %>%
  arrange(little_mix, radiohead) %>%
  left_join(get_sentiments("afinn")) %>%
  mutate(score = factor(sign(ifelse(is.na(score), 0, score))))

# right let's plot these % on a log log scale
# we'll highlight those with a pos/neg sentiment in red/green and bold
# whilst the 0'ers will be light grey
frequency %>%
  ggplot(aes(little_mix, radiohead, colour = score)) +
  scale_colour_manual(values = c("firebrick1", "grey81", "springgreen4")) +
  geom_jitter( alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word, fontface = "bold"), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  theme(legend.position = "none") +
  geom_abline(color = "red") +
  labs(title = "Comparing Word Frequencies between Little Mix and Radiohead",
       x = "Frequency of Occurrence in Little Mix Songs",
       y = "Frequency of Occurrence in Radiohead Songs")

# Little Mix do love love
# now a bunch of stuff for the log ratio's between the two bands
# which words are inordinately more likely to appear in that band's lyric
word_ratios <- both_bands %>%
  count(word, band) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(band, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(radiohead / little_mix)) %>%
  arrange(desc(logratio))

word_ratios %>% 
  arrange(abs(logratio))

word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (radiohead/little_mix)") +
  scale_fill_discrete(name = "", labels = c("radiohead", "little_mix"))

# little mix have some really short yelps and stuff in there
# tbf it's not very interesting and i won't probably use it
