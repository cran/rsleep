#' Deep Learning Architecture for Temporal Sleep Stage Classification model implementation in Keras.
#'
#' @description Keras implementation of the deep learning architecture described by Chambon & Al in "A Deep Learning Architecture for Temporal Sleep Stage Classification Using Multivariate and Multimodal Time Series". Consecutives polysomnography (PSG) epochs are supposed to be input to the model to fit on categorized stages as output. `write_batches_psg()` function writes files batches with the right format for `x` and `y` values. The model can then be trained using the `train_batches()` function. `score_psg()` uses this model to predict PSG epochs from a raw European Data Format (EDF) record.
#' @references Chambon, S., Galtier, M., Arnal, P., Wainrib, G. and Gramfort, A. (2018) A Deep Learning Architecture for Temporal Sleep Stage Classification Using Multivariate and Multimodal Time Series. IEEE Trans. on Neural Systems and Rehabilitation Engineering 26:(758-769).
#' @param channels Integer. Number of channels in each input.
#' @param samples Integer. Number of samples in each channel.
#' @return A Keras sequential model.
#' @export
chambon2018 <- function(
  channels = 6,
  samples = 6300){
  
  model <- keras::keras_model_sequential()
  
  model <- keras::layer_conv_2d(
    model,
    filters = channels,
    kernel_size = c(1,32),
    activation = 'linear',
    input_shape = c(channels,samples,1))
  
  model <- keras::layer_conv_2d(
    model, filters = 32, kernel_size = c(1,32), activation = 'relu')
  
  model <- keras::layer_max_pooling_2d(
    model, pool_size = c(1, 8))
  
  model <- keras::layer_conv_2d(
    model,filters = 32, kernel_size = c(1,32), activation = 'relu')
  
  model <- keras::layer_max_pooling_2d(model,pool_size = c(1, 8))
  
  model <- keras::layer_conv_2d(
    model,filters = 32, kernel_size = c(1,32), activation = 'relu')
  
  model <- keras::layer_max_pooling_2d(model,pool_size = c(1, 8))
  
  model <- keras::layer_flatten(model)
  model <- keras::layer_dense(model,units = 512, activation = 'relu')
  model <- keras::layer_dropout(model,rate = 0.5)
  model <- keras::layer_dense(model, units = 5, activation = 'softmax')
  
  keras::compile(model,
                 loss = "categorical_crossentropy",
                 optimizer = keras::optimizer_adam(
                   lr = 0.0001, decay = 1e-6),
                 metrics = "accuracy")
  
  model
}

#' Automated Classification of Sleep Stages in Mice with Deep Learning model implementation in Keras.
#'
#' @description Model inspired by the article "Automated Classification of Sleep Stages and EEG Artifacts in Mice with Deep Learning". Implemented using Keras. Adapted to use minimum 2 channels and to not score artifact epochs.
#' @references Schwabedal, Justus T. C., Daniel Sippel, Moritz D. Brandt, and Stephan Bialonski. “Automated Classification of Sleep Stages and EEG Artifacts in Mice with Deep Learning.” ArXiv:1809.08443 [Cs, q-Bio], September 22, 2018. http://arxiv.org/abs/1809.08443.
#' @param channels Number of channels in each input.
#' @param samples Number of samples in each channel.
#' @return A Keras sequential model.
#' @export
schwabedal2018 <- function(channels = 2,
                           samples = 8000){
  
  model <- keras::keras_model_sequential()
  
  # Layer 1
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(channels,5),
    activation = 'relu', input_shape = c(channels, samples, 1),
    strides = c(1,1))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 2
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1, 5),
    activation = 'relu', strides = c(2,2))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 3
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(1,1))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 4
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(2,2))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 5
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(1,1))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 6
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(2,2))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 7
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(1,1))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 8
  model <- keras::layer_conv_2d(
    model, filters = 64, kernel_size = c(1,5),
    activation = 'relu', strides = c(2,2))
  
  model <- keras::layer_batch_normalization(model)
  
  # Layer 9
  model <- keras::layer_flatten(model)
  
  model <- keras::layer_dense(model,units = 80, activation = 'relu')
  
  model <- keras::layer_dense(model, units = 3, activation = 'softmax')
  
  keras::compile(
    model,
    loss = "categorical_crossentropy",
    optimizer = keras::optimizer_rmsprop(lr = 0.0001),
    metrics = "categorical_accuracy")
  
  model
  
}