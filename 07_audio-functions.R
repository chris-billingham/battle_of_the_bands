# take in an mp3 and convert it into spectrograms covering 1 second, save the spectrogram to a directory
# in here this function is to an IMAGES directory, used for initial setup
chunk_and_save <- function(filename) {
  
  r <- readMP3(filename)
  
  album <- filename %>%
    str_replace("mp3/", "") %>%
    str_extract("^[a-z0-9_]+")
  
  track_name <- filename %>%
    str_extract("[A-Za-z0-9 -()_]+.mp3") %>%
    trimws() %>%
    str_replace("[0-9]{2} - ", "") %>%
    str_replace(".mp3", "") %>%
    str_replace_all(" ", "_") %>%
    tolower()
  
  # convert to mono from stereo, averaging both channels
  mono_r <- mono(r, "both")
  
  # get the duration for the entire file, we'll need this for later
  duration_total <- round(length(mono_r@left) / mono_r@samp.rate, 2)
  print(filename)
  
  pb <- progress_bar$new(
    format = " processing mp3 file [:bar] :percent :elapsedfull",
    clear = FALSE,
    total = floor(duration_total)
  )
  
  for (i in 0:(floor(duration_total)-1)) {
    chunk <- extractWave(mono_r, from = i, to = i+1, xunit = "time")
    chunk_name <- paste0("plot/", album, "/", track_name, str_pad(as.character(i), 4, side = "left", pad = "0"), ".png")
    save_spectrogram(chunk, chunk_name)
    pb$tick()
  }

}

# process a single chunk of music and save off image at a named location
save_spectrogram <- function(wave_obj, filename) {
  
  # extract the left channel
  r_1s <- wave_obj@left
  
  # determine sample rate
  fs <- wave_obj@samp.rate
  
  # determine duration
  duration <- length(r_1s)/fs
  
  # demean to remove DC offset
  snd <- r_1s - mean(r_1s)
  
  # number of points to use for the fft
  nfft <- 1024
  
  # window size (in points)
  window <- 128
  
  # overlap (in points)
  overlap <- 64
  
  
  # create spectrogram
  spec <- specgram(x = r_1s,
                   n = nfft,
                   Fs = fs,
                   window = window,
                   overlap = overlap
  )
  
  # discard phase information
  P <- abs(spec$S)
  
  # normalize, but only if we're not in silence
  if(!(mean(P) == 0)) {
    P <- P/max(P)
  }
  
  # convert to dB, again but not if we're in silence
  if(!(mean(P) == 0)) {

  P <- 10*log10(P)

  }
  
  # config time axis
  t <- spec$t
  
  # plot spectrogram and save
  if(!(mean(P) == 0)) {

  png(filename)
  imagep(x = t,
         y = spec$f,
         z = t(P),
         col = oce.colorsVelocity,
         drawPalette = FALSE,
         decimate = FALSE,
         axes = FALSE
  )
  dump <- dev.off()
}
}

# this is the same as chunk and save but saves into the tempdir() for individual track prediction
chunk_and_temp <- function(filename) {
  
  r <- readMP3(filename)
  
  album <- filename %>%
    str_replace("mp3/", "") %>%
    str_extract("^[a-z0-9_]+")
  
  track_name <- filename %>%
    str_extract("[A-Za-z0-9 -()_]+.mp3") %>%
    trimws() %>%
    str_replace("[0-9]{2} - ", "") %>%
    str_replace(".mp3", "") %>%
    str_replace_all(" ", "_") %>%
    tolower()
  
  # convert to mono from stereo, averaging both channels
  mono_r <- mono(r, "both")
  
  # get the duration for the entire file, we'll need this for later
  duration_total <- round(length(mono_r@left) / mono_r@samp.rate, 2)
  print(paste0("Converting ",basename(filename), " into ", floor(duration_total), " spectrograms"))
  
  pb <- progress_bar$new(
    format = " processing mp3 file [:bar] :percent eta: :eta",
    clear = FALSE,
    total = floor(duration_total)
  )

  for (i in 0:(floor(duration_total)-1)) {
    chunk <- extractWave(mono_r, from = i, to = i+1, xunit = "time")
    chunk_name <- paste0(tempdir(), "/", track_name, str_pad(as.character(i), 4, side = "left", pad = "0"), ".png")
    save_spectrogram(chunk, chunk_name)
    pb$tick()
  }

}
