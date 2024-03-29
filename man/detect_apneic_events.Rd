% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resp.R
\name{detect_apneic_events}
\alias{detect_apneic_events}
\title{Detect Apneic Events in SpO2 Signal Data}
\usage{
detect_apneic_events(spo2, sRate)
}
\arguments{
\item{spo2}{A numeric vector representing the SpO2 signal data.}

\item{sRate}{The original sampling rate of the SpO2 signal.}
}
\value{
A list of numeric vectors. Each vector represents a detected apneic event, containing the start and end
        indices of the event in the resampled signal.
}
\description{
This function implements the algorithm described by Jund & Al in "Real-Time Automatic Apneic Event Detection Using Nocturnal Pulse Oximetry", 2018.
It analyzes a given SpO2 signal to detect apneic events. It works by resampling the input signal
and applying a series of checks to identify potential apnea instances. The algorithm uses a state machine
with different blocks representing various stages of detection.
}
\examples{
# Example usage
spo2_sample <- c(98, 97, 96, 95, 94, 93, 92, 91, 90, 89, 88)
sample_rate <- 1  # Assuming 1 Hz sampling rate
detected_apneas <- detect_apneic_events(spo2_sample, sample_rate)
print(detected_apneas)

}
\references{
Jung, D. W., Hwang, S. H., Cho, J. G., Choi, B. H., Baek, H. J., Lee, Y. J., Jeong, D.-U., & Park, K. S. (2018). Real-Time Automatic Apneic Event Detection Using Nocturnal Pulse Oximetry. In IEEE Transactions on Biomedical Engineering (Vol. 65, Issue 3, pp. 706–712). Institute of Electrical and Electronics Engineers (IEEE). https://doi.org/10.1109/tbme.2017.2715405
}
