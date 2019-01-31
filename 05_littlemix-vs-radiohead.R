library(tidyverse)

littlemix_uni_sent$band <- "little_mix"
radiohead_uni_sent$band <- "radiohead"
both_bands <- bind_rows(littlemix_uni_sent, radiohead_uni_sent) %>%
  anti_join(stop_words) %>%
  filter(!(word %in% c("jesy", "leigh", "anne", "jade", "perrie")))


frequency <- both_bands %>% 
  group_by(band) %>% 
  count(word, sort = TRUE) %>% 
  left_join(both_bands %>% 
              group_by(band) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

library(tidyr)

frequency <- frequency %>% 
  select(band, word, freq) %>% 
  spread(band, freq) %>%
  arrange(little_mix, radiohead)

library(scales)

ggplot(frequency, aes(little_mix, radiohead)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")

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

