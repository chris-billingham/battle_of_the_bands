library(tidyverse)
library(keras)
library(tuneR)
library(signal)
library(oce)
library(fs)
library(progress)
library(pbapply)

# load in the audio functions
source("07_audio-functions.R")

# load in the model
model <- load_model_hdf5("models/bob_cnn_dropout.h5")

# get the album in the right order
bands <- c("little mix", "radiohead")

# image prediction function
image_predict <- function(file_path) {
  
  # process image
  img <- image_load(file_path, target_size = c(480, 480)) %>%
    image_to_array() %>%
    array_reshape(dim = c(1, 480, 480, 3))
  
  # rescale
  img <- img / 255
  
  # predict
  predict <- model %>% predict(img)
  
  # which band, return that likelhood
  band_predict <- tibble(is_radiohead = predict)
  
  return(band_predict)
}

# entire prediction loop
predict_band <- function(mp3_file_path) {
  
  # take the mp3 file path and turn it into 1 second spectrograms in a temp directory
  chunk_and_temp(mp3_file_path)
  
  # get the list of images
  image_files <- dir_ls(tempdir(), type = "file", glob = "*.png")
  
  # a note to tell us what's happening
  print(paste("Predicting likelihood of radiohead for each spectrogram"))
  
  # run the loop through the entire directory of images from the track
  radiohead_prediction <- pblapply(image_files, image_predict) %>% 
    bind_rows() %>%
    mutate(winner = ifelse(is_radiohead > 0.5, 1, 0),
           file = mp3_file_path,
           pos = row_number())
  
  # return a single prediction band, but i won't use this.
  band_prediction <- tibble(file = mp3_file_path,
                            pct_radiohead = sum(radiohead_prediction$winner)/nrow(radiohead_prediction))
  
  
  print("Collating all spectrogram results into single track likelihood")
  
  # cleaning up my own filth
  file_delete(image_files)
  
  # send it on back
  return(radiohead_prediction)
}

# get all the heldout mp3s
all_tracks <- dir_ls("holdout", recursive = TRUE, type = "file", glob = "*.mp3")

# create a tibble
all_tracks_df <- tibble(track_name = all_tracks)

# let's predict the band, and see how long it takes
# WARNING - This is slow as. A 4 min track can take over 7 mins to run
# so an entire album can be a couple of hours
# sit back, relax and enjoy my progress bars
predictions_all <- map_dfr(all_tracks_df$track_name, predict_band)

# lets make a summary table per track
summary <- predictions_all %>% 
  group_by(file) %>% 
  summarise(n_prob = sum(is_radiohead), 
            n_winner = sum(winner), n = n())

# combine the lot of with some probably unnecessarily long regex to get it neat and tidy
radiomix <- predictions_all %>%
  mutate(band = case_when(grepl("moon_shaped_pool", file)  ~ "radiohead",
                          TRUE ~ "little mix"),
         track_number = as.numeric(str_extract(gsub("lm5", "lmfive", file), "[0-9]+")),
         track_name = gsub(".mp3", "", trimws(str_extract(file, "([A-Za-z'\\(\\) ]+).mp3"))))

# display the radioheadedness of each track on a moon shaped pool
# pink being turbo little mix and dark grey being turbo radiohead
radiomix %>% 
  dplyr::filter(band == "radiohead") %>%
  ggplot(aes(pos, is_radiohead, colour = winner)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_colour_gradient(low = "deeppink2", high = "grey35") +
  facet_wrap(~ fct_reorder(track_name, track_number), scales = "free_x") +
  labs(title = "A Moon Shaped Pool - Radiohead",
       subtitle = "Radiohead-Little Mix'edness",
       x = "seconds of the track",
       y = "Radiohead Ratio")

# hey, check out identikit, it actually comes in at 0.47 or so, so it's officially a little mix track!

# same for little mix
radiomix %>% 
  dplyr::filter(band == "little mix") %>%
  ggplot(aes(pos, is_radiohead, colour = winner)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_colour_gradient(low = "deeppink2", high = "grey35") +
  facet_wrap(~ fct_reorder(track_name, track_number), scales = "free_x") +
  labs(title = "LM5 - Little Mix",
       subtitle = "Radiohead-Little Mix'edness",
       x = "seconds of the track",
       y = "Radiohead Ratio")

# yeah, little mix are, well pretty little mix
