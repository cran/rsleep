% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/events.R
\name{check_events}
\alias{check_events}
\title{Check events dataframe format compliance.}
\usage{
check_events(events)
}
\arguments{
\item{events}{Events dataframe. Dataframe must contain \code{begin} (\code{POSIXt}), \code{end} (\code{POSIXt}) and \code{event} (\code{character}) columns.}
}
\value{
Boolean, according to the events dataframe format compliance.
}
\description{
Check events dataframe format compliance.
}
\examples{
events <- data.frame(begin = as.POSIXct(c(1536967800,1536967830,1536967860), origin = "1970-01-01"))
events$end <- as.POSIXct(c(1536967830,1536967860,1536967890), origin = "1970-01-01")
events$event = c("N3","N3","REM")
rsleep::check_events(events)
}
