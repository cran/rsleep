% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/events.R
\name{plot_hypnodensity}
\alias{plot_hypnodensity}
\title{Plot a hypnodensity graph.}
\usage{
plot_hypnodensity(
  hypnodensity,
  stages = c("AWA", "REM", "N1", "N2", "N3"),
  colors = c("#5BBCD6", "#FF0000", "#00A08A", "#F2AD00", "#F98400")
)
}
\arguments{
\item{hypnodensity}{A hypnodensity dataframe as returned by the `score_stages_edf` function.}

\item{stages}{Vector of stages labels to plot.}

\item{colors}{Vector of colors to use.}
}
\value{
A `ggplot2` hypnodensity graph.
}
\description{
Plot a hypnodensity graph using `ggplot2`. Hypnodensity can be read from file or returned by the `score_stages_edf` function.
}
\examples{
tryCatch({
download.file("https://rsleep.org/data/hypnodensity.csv", "hypnodensity.csv")

hypnodensity <- read.csv2("hypnodensity.csv")

unlink("hypnodensity.csv")

plot_hypnodensity(hypnodensity)
}, error = function(e) {
  print("Error executing this example, check your internet connection.")
  })
}
\references{
Stephansen, J.B., Olesen, A.N., Olsen, M., Ambati, A., Leary, E.B., Moore, H.E., Carrillo, O., Lin, L., Han, F., Yan, H. and Sun, Y.L., 2018. Neural network analysis of sleep stages enables efficient diagnosis of narcolepsy. Nature communications, 9(1), p.5229.
}
