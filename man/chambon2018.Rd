% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{chambon2018}
\alias{chambon2018}
\title{Deep Learning Architecture for Temporal Sleep Stage Classification model implementation in Keras.}
\usage{
chambon2018(channels = 6, samples = 6300)
}
\arguments{
\item{channels}{Integer. Number of channels in each input.}

\item{samples}{Integer. Number of samples in each channel.}
}
\value{
A Keras sequential model.
}
\description{
Keras implementation of the deep learning architecture described by Chambon & Al in "A Deep Learning Architecture for Temporal Sleep Stage Classification Using Multivariate and Multimodal Time Series". Consecutives polysomnography (PSG) epochs are supposed to be input to the model to fit on categorized stages as output. `write_batches_psg()` function writes files batches with the right format for `x` and `y` values. The model can then be trained using the `train_batches()` function. `score_psg()` uses this model to predict PSG epochs from a raw European Data Format (EDF) record.
}
\references{
Chambon, S., Galtier, M., Arnal, P., Wainrib, G. and Gramfort, A. (2018) A Deep Learning Architecture for Temporal Sleep Stage Classification Using Multivariate and Multimodal Time Series. IEEE Trans. on Neural Systems and Rehabilitation Engineering 26:(758-769).
}
