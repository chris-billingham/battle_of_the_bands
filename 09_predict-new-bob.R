library(tidyverse)
library(keras)
library(tuneR)
library(signal)
library(oce)
library(fs)
library(progress)

# load in the audio functions
source("02_audio-functions.R")

# load in the model
model <- load_model_hdf5("models/blur_cnn_dropout_2.h5")

# get the album in the right order
albums <- dir_ls("mp3/", type = "directory") %>%
  gsub("mp3/", "", .) %>%
  as.character()

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
  album_predict <- data.frame(album = albums,
                              likelihood = t(predict),
                              stringsAsFactors = FALSE) %>%
    mutate(winner = ifelse(likelihood == max(likelihood), 1, 0))
  
  
  return(album_predict)
}

# entire prediction loop
predict_blur_album <- function(mp3_file_path) {
  
  # take the mp3 file path and turn it into 1 second spectrograms in a temp directory
  chunk_and_temp(mp3_file_path)
  
  # get the list of images
  image_files <- dir_ls(tempdir(), type = "file", glob = "*.png")
  
  # a note to tell us what's happening
  print(paste("Predicting likelihood of album for each spectrogram"))
  
  
  album_prediction <- pblapply(image_files, image_predict) %>% 
    bind_rows() %>%
    group_by(album) %>%
    summarise(total_img_likelihood = sum(likelihood),
              total_winner = sum(winner)) %>%
    mutate(album_likelihood = total_img_likelihood/sum(total_img_likelihood),
           album_winner = total_winner/sum(total_winner)) %>%
    arrange(desc(album_likelihood))
  
  print("Collating all spectrogram results into single track likelihood")
  file_delete(image_files)
  
  return(album_prediction)
}

# get all the b-sides from the blur 21 boxset
all_tracks <- dir_ls("~/Downloads/Blur 21", recursive = TRUE, type = "file", glob = "*.mp3")
all_tracks_df <- data_frame(track_name = all_tracks) %>%
  dplyr::filter(grepl("CD2", track_name))

# randomly choose a track and it's path from the list of tracks
random_track <- as.character(all_tracks_df[floor(runif(1, 1, nrow(all_tracks_df))),1])

# let's predict the album, and see how long it takes
predict_blur_album(random_track)


