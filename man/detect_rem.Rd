% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eog.R
\name{detect_rem}
\alias{detect_rem}
\title{Detection of Rapid-Eye Movements (REMs)}
\usage{
detect_rem(
  roc,
  loc,
  sRate,
  l = 0.5,
  art = 500,
  nip = 120,
  cc = -0.2,
  da = 45,
  dadiff = 15
)
}
\arguments{
\item{roc}{Right outer canthus EOG signal vector.}

\item{loc}{Left outer canthus EOG signal vector.}

\item{sRate}{Sampling rate of the EOG signals in Hz.}

\item{l}{Window length in seconds for REM detection (default is 0.5).}

\item{art}{Artifact threshold, max amplitude allowed in EOG signals to consider the data valid (default is 500).}

\item{nip}{Negative Inflexion Point threshold for REM detection (default is 120).}

\item{cc}{Correlation coefficient threshold for inclusion of a REM event. Negative correlation 
indicates potential REM (default is -0.2).}

\item{da}{Desired angle for REM detection (default is 45 degrees).}

\item{dadiff}{Acceptable deviation from the desired angle for one eye, if the other eye compensates 
with a larger deviation (default is 15 degrees).}
}
\value{
A list with the following elements:
  - `rocf`: Filtered right outer canthus EOG signal.
  - `locf`: Filtered left outer canthus EOG signal.
  - `block_art`: Maximum absolute amplitude in the EOG channels for the detection block, used for artifact measurement.
  - `cpm`: Product of inverted left and right EOG signals, part of REM detection criteria.
  - `cn`: Conditioned signal based on `cpm`, with values below a threshold set to 0.
  - `crems`: Data frame of candidate REMs with indices, characteristics, and validity flag.
  - `rems`: Subset of `crems` containing only the valid REM events.
}
\description{
Implements the algorithm detailed in Agarwal &Al. "Detection of Rapid-Eye Movements in Sleep Studies."
This function processes electrooculography (EOG) signals to detect rapid-eye 
movements (REMs) characteristic of REM sleep, applying filters, artifact detection, and angle-based 
inclusion criteria.
}
\details{
The function processes the EOG signals by applying a band-pass Butterworth filter to isolate 
  frequencies between 1 and 5 Hz, typical for REMs. It then computes the artifact measure and evaluates 
  the signal for REM events based on the slope of the EOG signal segments, correlation between left and 
  right signals, and other criteria derived from the REM detection algorithm described by Agarwal et al.
  The function returns a list containing filtered EOG signals, artifact measures, and detected REM events
  with their characteristics and validity based on the algorithm's criteria.
}
\references{
Agarwal, R., Takeuchi, T., Laroche, S., & Gotman, J. (2005). Detection of Rapid-Eye Movements in 
Sleep Studies. IEEE Transactions on Biomedical Engineering, 52(8), 1390–1396. 
https://doi.org/10.1109/TBME.2005.851512
}
