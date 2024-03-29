% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resp.R
\name{clean_oximetry}
\alias{clean_oximetry}
\title{Clean Oximetry Signal}
\usage{
clean_oximetry(oximetry, threshold = 70)
}
\arguments{
\item{oximetry}{A numeric vector representing the oximetry signal. 
Each element corresponds to an oximetry reading.}

\item{threshold}{A numeric value setting the minimum acceptable oximetry value. 
Default is 70. Values in `oximetry` below this threshold will be 
replaced with the nearest value above the threshold or an average 
of the nearest valid values on either side.}
}
\value{
A numeric vector of the same length as `oximetry`. 
        Sub-threshold values are replaced based on nearby valid readings.
}
\description{
This function processes an oximetry signal vector to remove values below a specified threshold. 
It is designed to enhance the quality of oximetry data by replacing sub-threshold impossible values 
with the nearest valid data points.
}
\details{
The function iterates through the `oximetry` vector. For each value below the `threshold`,
it searches for the nearest valid value (above the threshold) to the left and right. 
If both neighbors are found, it replaces the sub-threshold value with their average. 
If only one valid neighbor is found, it uses that value. 

The algorithm ensures that the processed signal retains the general pattern of the 
original data while mitigating the impact of anomalously low readings.
}
\examples{
oximetry_data <- c(91, 92, 91, 34, 92, 93, 91)
clean_oximetry(oximetry_data)

}
