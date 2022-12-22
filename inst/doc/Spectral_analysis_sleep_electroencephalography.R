## ----env, include = FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----edf_read, echo=FALSE-----------------------------------------------------
library(edfReader)

if(!file.exists("learn-nsrr01.edf")){
  download.file(
  url = "https://sleepdata.org/datasets/learn/files/m/browser/polysomnography/edfs/learn-nsrr01.edf",
  destfile = "learn-nsrr01.edf")}

h <- readEdfHeader("learn-nsrr01.edf")

s <- readEdfSignals(h)

## ----spectrogram, echo=FALSE, fig.width=7-------------------------------------
library(rsleep)

spectrogram(
  signal = s$EEG$signal,
  sRate = s$EEG$sRate,
  startTime = s$ECG$startTime)

## ----hypnogram, echo=FALSE, fig.width = 7-------------------------------------
if(!file.exists("learn-nsrr01-profusion.xml")){
  download.file(
  url = "https://sleepdata.org/datasets/learn/files/m/browser/polysomnography/annotations-events-profusion/learn-nsrr01-profusion.xml",
  destfile = "learn-nsrr01-profusion.xml")}

events <- read_events_profusion(
  "learn-nsrr01-profusion.xml", h$startTime)

plot_hypnogram(events)

## ----epoching-----------------------------------------------------------------
epochs <- epochs(
  signals = s$EEG$signal,
  sRates = s$EEG$sRate,
  epoch = rsleep::hypnogram(events),
  startTime = as.numeric(as.POSIXct(h$startTime)))

## ----pwelch, fig.width=7, message=FALSE, error=FALSE--------------------------
p <- pwelch(epochs[[100]], sRate = s$EEG$sRate)

summary(p)

## ----avg_pdg_compute----------------------------------------------------------
periodograms <- mapply(x = epochs, y = rsleep::hypnogram(events)$event, FUN = function(x,y){
  p <- pwelch(x, sRate = s$EEG$sRate, show = FALSE)
  p <- as.data.frame(p[p$hz <= 30,])
  p$stage <- y
  p
}, SIMPLIFY = F)

## ----pdg_rbind----------------------------------------------------------------
periodograms_df <- do.call("rbind", periodograms)

## ----pdg_aggregate------------------------------------------------------------
avg_periodograms <- aggregate(psd ~ hz+stage, periodograms_df, mean)

## ----periodogram_plot, fig.width=7, message=FALSE, error=FALSE----------------
library(ggplot2)

palette <- c("#F98400","#F2AD00","#00A08A","#FF0000","#5BBCD6")

ggplot(avg_periodograms, aes(x=hz,y=psd,color=stage)) +
  geom_line() + theme_bw() +
  theme(legend.title = element_blank()) + 
  scale_colour_manual(name = "stage",
                      values = palette) +
  xlab("Frequency (Hertz)") + ylab("PSD")

## ----bands_compute------------------------------------------------------------
bands <- lapply(epochs,function(x){
    bands_psd(
      bands = list(c(0.5,3.5), # Delta
                   c(3.5,7.5), # Theta
                   c(7.5,13), # Alpha
                   c(13,30)), # Beta
      signal = x, sRate = s$EEG$sRate,
      normalize = c(0.5,30))
})

## ----bands_reshape------------------------------------------------------------
bands_df <- data.frame(matrix(unlist(bands), nrow=length(bands), byrow=TRUE))

colnames(bands_df) <- c("Delta","Theta","Alpha","Beta")

## ----bands_stages-------------------------------------------------------------
bands_df$stage <- rsleep::hypnogram(events)$event

## ----bands_plot, fig.width=7, fig.height=7, message=FALSE, error=FALSE--------
bands_df_long <- reshape2::melt(bands_df, "stage")

palette <-c("#F98400", "#F2AD00", "#00A08A", "#FF0000", "#5BBCD6")

ggplot(bands_df_long,
       aes(x=stage,y=value,color=stage)) +
  geom_boxplot() +
  facet_grid(rows = vars(variable),scales = "free") +
  scale_colour_manual(name = "stage",
                      values = palette) +
  theme_bw() + xlab("") + ylab("PSD") + 
  theme(legend.position = "none")

