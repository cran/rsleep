% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{stages_stats}
\alias{stages_stats}
\title{Get stages related statistics in a named vector.}
\usage{
stages_stats(e)
}
\arguments{
\item{e}{Events dataframe. Dataframe must have \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.}
}
\value{
stages vector
}
\description{
\code{stages_stats} computes stages related statistics.
}
\examples{
e <- data.frame(begin = as.POSIXlt(seq(from = 0, to = 30*10, by = 30),origin = "1970-01-01"))
e$end <- as.POSIXlt(seq(from = 30, to = 30*11, by = 30), origin = "1970-01-01")
e$event = c("AWA","N1","N2","N3","N3","REM","N2","REM","N2","REM","AWA")
stages_stats(e)
}
