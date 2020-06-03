context("EEG signal functions testing")

test_that("Spectrogram", {

  library(signal)

  sig <- chirp(seq(-2, 15, by = 0.001), 400, 10, 100, 'quadratic')

  spectrogram(signal = sig,
                      sRate = 200,
                      n=2048,
                      window = 2048,
                      plot=TRUE)
  if(file.exists("Rplots.pdf")){
    file.remove("Rplots.pdf")
  }

  spec <- spectrogram(signal = sig,
                      sRate = 200,
                      n=2048,
                      window = 2048,
                      plot=FALSE)

  expect_equal(class(spec),"specgram")
})

test_that("Bands", {

  library(signal)

  sig <- chirp(seq(-2, 15, by = 0.001), 400, 10, 100, 'quadratic')

  bands <- bands_power(bands = list(c(0.3,4),c(1,2)),signal = sig,sRate = 200)

  expect_equal(length(bands),2)
})

test_that("psd", {
  x <- sin(c(1:10000))
  psd <- psm(x, 200, 100)
  expect_equal(as.character(psd[[2]][49]),"48.022538298763")
  psd <- pwelch(x, 200, 100)
  expect_equal(as.character(psd[[2]][49]),"-14.6969137350995")
})
