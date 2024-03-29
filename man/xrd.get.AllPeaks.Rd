% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xrd.get.AllPeaks.R
\name{xrd.get.AllPeaks}
\alias{xrd.get.AllPeaks}
\title{Finds all the peaks in a spectrum}
\usage{
xrd.get.AllPeaks(
  TwoTheta,
  Intensity,
  min.Prominence = 5,
  Try.Sigma = c(0.2, 0.1, 0.05, 0.3),
  deltaTheta = 5,
  Range = c(0, 90),
  verbose = FALSE
)
}
\arguments{
\item{TwoTheta}{angle}

\item{Intensity}{intensity signal}

\item{min.Prominence}{minimum prominence in percent (optional)}

\item{Try.Sigma}{vector with peak widths used to start fitting (optional)}

\item{deltaTheta}{search area around main peak}

\item{Range}{range to search for peaks (optional)}

\item{verbose}{logical, if \code{TRUE} outputs additional info}
}
\value{
vector with peak positions
}
\description{
Finds all the peaks in a spectrum
}
\examples{

\donttest{
filename = xrd.getSampleFiles()[1]
d = xrd.read.ASC(filename)
peak.list = xrd.get.AllPeaks(d$theta, d$I)
}

}
