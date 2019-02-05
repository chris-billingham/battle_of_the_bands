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
  
  # which album
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
  
  
  radiohead_prediction <- pblapply(image_files, image_predict) %>% 
    bind_rows() %>%
    mutate(winner = ifelse(is_radiohead > 0.5, 1, 0),
           file = mp3_file_path,
           pos = row_number())
  
  band_prediction <- tibble(file = mp3_file_path,
                            pct_radiohead = sum(radiohead_prediction$winner)/nrow(radiohead_prediction))
  
  
  print("Collating all spectrogram results into single track likelihood")
  file_delete(image_files)
  
  return(radiohead_prediction)
}

# get all the heldout mp3s
all_tracks <- dir_ls("holdout", recursive = TRUE, type = "file", glob = "*.mp3")
all_tracks_df <- data_frame(track_name = all_tracks)

# let's predict the band, and see how long it takes
predictions_all <- map_dfr(all_tracks_df$track_name, predict_band)

predictions_all %>% 
  group_by(file) %>% 
  summarise(n_prob = sum(is_radiohead), 
            n_winner = sum(winner), n = n())



