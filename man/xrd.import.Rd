% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xrd.import.R
\name{xrd.import}
\alias{xrd.import}
\title{Imports Rigaku x-ray diffraction data}
\usage{
xrd.import(filename, xrd = FALSE)
}
\arguments{
\item{filename}{full file name with path}

\item{xrd}{logical, if \code{TRUE} an xrd object is returned}
}
\value{
xrd object or data frame with XRD data
}
\description{
Import function recognizes ASC, TXT, RAS, and RASX files from Rigaku x-ray diffraction
instrument; returns an xrd object or data frame with 2theta,
I (intensity normalized for time) and I.meas (measured intensity).
}
\examples{
fname = xrd.getSampleFiles('rasx')
d = xrd.import(fname, TRUE)
plot(d)

}
\author{
Thomas Gredig
}
