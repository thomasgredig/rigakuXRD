% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xrd.get.DebyeScherrer.R
\name{xrd.get.DebyeScherrer}
\alias{xrd.get.DebyeScherrer}
\title{Applies Scherrer Equation to PeakStats}
\usage{
xrd.get.DebyeScherrer(PeakStats, Lambda = 1.5406, K = 0.9)
}
\arguments{
\item{PeakStats}{vector with the peak statistics}

\item{Lambda}{wavelength of x-ray in (A)}

\item{K}{geometrical factor, usually a number between 0.9 and 1.0}
}
\value{
peak width in Angstrom
}
\description{
Returns the particle size or film thickness as calculated with
the Scherrer equation; units of Angstrom, for more information
see [Scherrer Equation](https://en.wikipedia.org/wiki/Scherrer_equation)
}
\examples{
filename = xrd.getSampleFiles()[1]
d = xrd.read.ASC(filename)
ds = xrd.get.PeakStats(d$theta, d$I, 38.2)
xrd.get.DebyeScherrer(ds)

}
\seealso{
[xrd.get.PeakStats()]
}
