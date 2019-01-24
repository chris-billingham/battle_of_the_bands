library(tidyverse)
library(keras)

# here we're going to use a CNN and stick in a tonne of drop out
# as it'd be very easy for this to overfit with the cyclicness of the music
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", input_shape = c(480, 480, 3)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(filters = 256, kernel_size = c(3, 3), activation = "relu") %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%  
  layer_dropout(rate = 0.25) %>% 
  layer_flatten() %>% 
  layer_dense(units = 128, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 1, activation = 'sigmoid')

summary(model)

# mixing things up with adadelta for a change
model %>% compile(
  loss = "binary_crossentropy",
  optimizer = optimizer_adadelta(),
  metrics = c("accuracy")
)

# create the image data generators, we're just going to use rescale here
train_datagen <- image_data_generator(rescale = 1/255)
validation_datagen <- image_data_generator(rescale = 1/255)

# let's do this for the train
train_generator <- flow_images_from_directory(
  directory = "images/train",
  generator = train_datagen,
  target_size = c(480, 480),
  batch_size = 20,
  class_mode = "categorical"
)

# and the validation
validation_generator <- flow_images_from_directory(
  directory = "images/validate",
  generator = validation_datagen,
  target_size = c(480, 480),
  batch_size = 20,
  class_mode = "categorical"
)

# let's train this cnn! putting in a bunch of epochs as its a slow converger
# on cpu it's like 13min per epoch, on gpu about 45s
history <- model %>% fit_generator(
  train_generator,
  steps_per_epoch = 100,
  epochs = 100,
  validation_data = validation_generator,
  validation_steps = 50
)

# there were other models this is the winner
model %>% save_model_hdf5("models/bob_cnn_dropout.h5")

# let's look at our chart
plot(history)

