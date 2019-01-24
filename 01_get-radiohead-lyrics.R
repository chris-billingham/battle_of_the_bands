options(stringsAsFactors = FALSE)
library(tidyverse)

# first let's scrape wikipedia for the list of Radiohead Studio albums
library(rvest)
url <- "https://en.wikipedia.org/wiki/Radiohead_discography#Studio_albums"

# get the list of full studio albums for radiohead from wikipedia
radiohead_albums <- read_html(url) %>% 
  html_nodes("table.wikitable.plainrowheaders") %>%
  html_table(fill = TRUE) %>%
  .[[1]] %>%
  slice(2:(n()-1)) %>%
  .[,1] %>%
  tolower()

# now we're going to use the geniusR package and get all the lyrics in a list
library(geniusR)

# annoyingly genius_album doesn't return the album name so i need to supplement this
genius_album_plus <- function(artist, album) {
  df <- genius_album(artist, album) %>%
    mutate(album = album)
  return(df)
}

# let's use this amended function to create a single datafame
radiohead_lyrics <- radiohead_albums %>%
  map2_dfr("radiohead", ., genius_album_plus)

# we'll need this for later
radiohead_albums_df <- tibble(album = radiohead_albums) %>%
  mutate(album_no = seq(1, length(radiohead_albums)))

