library(tidyverse)
library(tuneR)
library(signal)
library(oce)
library(caret)
library(fs)
library(progress)

# load in the audio functions
source("02_audio-functions.R")

# get a list of all blur mp3s from the mp3 directory
# note format in the directory is mp3/{album_name}/{track_number} - {track_name}.mp3
# so most of this is tuned for that
get_files <- fs::dir_ls("mp3/", recursive = TRUE, type = "file")

# create the directories for the plots, this could be better
if(!fs::dir_exists("plot/little_mix")){fs::dir_create("plot/little_mix")}
if(!fs::dir_exists("plot/radiohead")){fs::dir_create("plot/radiohead")}

# generate all the spectrograms and save to plot directories
walk(get_files, chunk_and_save)

# get all the new files into a big list
# NOTE using -size as there's a problem, i've had to raise a github issue for on the fs package
get_plots <- dir_info("plot/", recursive = TRUE, type = "file") %>%
  select(-size) %>% 
  mutate(album = str_extract(str_replace(path, "plot/", ""), "^[a-z_]+"))

# sort them into train and validate sets, create directories and then start copying files over
set.seed(1979)
train_index <- createDataPartition(get_plots$album, p = 0.75, list = FALSE)
train <- get_plots[train_index, ] %>%
  mutate(dataset = "train") %>%
  mutate(new_path = str_replace(path, "plot/", paste0("images/", dataset, "/")))

# sort out the new directory names and go and create them if needed
train %>% 
  transmute(new_dir = paste0("images/", dataset, "/", album)) %>% 
  unique() %>%
  walk(dir_create)

# copy over all the files from the old to the new
walk2(train$path, train$new_path, file_copy)

# same again for the validation set
validate <- get_plots[-train_index, ] %>%
  mutate(dataset = "validate") %>%
  mutate(new_path = str_replace(path, "plot/", paste0("images/", dataset, "/")))

# as before create the new directories
validate %>% 
  transmute(new_dir = paste0("images/", dataset, "/", album)) %>% 
  unique() %>%
  walk(dir_create)

# copy the files over
walk2(validate$path, validate$new_path, file_copy)

# right we're now ready for some DEEP LEARNING
