% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_pdrf.R
\name{calc_pdrf}
\alias{calc_pdrf}
\title{calculate pdrf parameters}
\usage{
calc_pdrf(
  pdet = NULL,
  dist = NULL,
  int = 5,
  slope = NULL,
  plot = FALSE,
  dmax = 1000,
  dint = 20
)
}
\arguments{
\item{pdet}{probability of detection}

\item{dist}{distance (in m) between transmitter and receiver}

\item{int}{intercept parameter from a logistic regression of distance on
p(detection). Default value is 5.}

\item{slope}{slope parameter from a logistic regression of distance on
p(detection). Default is NULL.}

\item{plot}{logical (default = FALSE); plot the relationship.}

\item{dmax}{maximum distance to be plotted. Ignored if plot = FALSE.}

\item{dint}{plotting interval between distances. Ignored if plot = FALSE.}
}
\value{
the intercept and slope parameters, given \code{pdet}, \code{dist}, and
one of \code{int} or \code{slope}.
}
\description{
A helper function to calculate the logisitc regression
intercept and/or slope parameters used to simulate acoustic detection
probability as a function of distance between transmitter and receiver
}
\details{
The \code{int} default value (5) is based on empirical data collected from
sentinel tags and receivers on the Ocean Tracking Network's Halifax line
(https://members.oceantrack.org/OTN/project?ccode=HFX).
}
\examples{
# calculate slope
calc_pdrf(pdet = 0.5, dist = 250, int = 5)

# calculate intercept
calc_pdrf(pdet = 0.5, dist = 250, slope = -0.02, int = NULL)

# calculate slope and plot relationship
calc_pdrf(pdet = 0.5, dist = 250, int = 5, plot = TRUE)

}
\author{
Ian Jonsen \email{ian.jonsen@mq.edu.au}
}
