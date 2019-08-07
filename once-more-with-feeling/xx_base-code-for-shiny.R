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

# mp3 file path, to be replaced with uploaded mp3
file_path <- dir_ls("holdout", recursive = TRUE, type = "file", glob = "*.mp3")

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
  
  # cleaning up my own filth
  file_delete(image_files)
  
  # send it on back
  return(radiohead_prediction)
}

# predict mp3
prediction <- predict_band(file_path)

# lets make a summary table per track
summary <- prediction %>% 
  group_by(file) %>% 
  summarise(n_prob = sum(is_radiohead), 
            n_winner = sum(winner), n = n())

# combine the lot of with some probably unnecessarily long regex to get it neat and tidy
radiomix <- prediction %>%
  mutate(track_name = gsub(".mp3", "", trimws(str_extract(file, "([A-Za-z'\\(\\) \\-]+).mp3"))))

# display the radioheadedness of each track on a moon shaped pool
# pink being turbo little mix and dark grey being turbo radiohead
radiomix %>% 
  ggplot(aes(pos, is_radiohead, colour = winner)) +
  geom_line() +
  theme(legend.position = "none") +
  scale_colour_gradient(low = "deeppink2", high = "grey35") +
  labs(title = file_path,
       subtitle = "Radiohead-Little Mix'edness",
       x = "seconds of the track",
       y = "Radiohead Ratio")

