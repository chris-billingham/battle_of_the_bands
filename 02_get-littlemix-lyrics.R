options(stringsAsFactors = FALSE)
library(tidyverse)

# first let's scrape wikipedia for the list of Little Mix Studio albums
library(rvest)
# now, let's brighten things up a bit
url <- "https://en.wikipedia.org/wiki/Little_Mix_discography#Studio_albums"

# get the list of full studio albums for littlemix from wikipedia
littlemix_albums <- read_html(url) %>% 
  html_nodes("table.wikitable.plainrowheaders") %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  slice(2:(n()-1)) %>%
  .[,1] %>%
  tolower()

# let's use this amended function to create a single datafame
littlemix_lyrics <- littlemix_albums %>%
  map2_dfr("little mix", ., genius_album_plus)

# we'll need this for later
littlemix_albums_df <- tibble(album = littlemix_albums) %>%
  mutate(album_no = seq(1, length(littlemix_albums)))
