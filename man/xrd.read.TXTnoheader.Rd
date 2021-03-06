% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xrd.read.TXTnoheader.R
\name{xrd.read.TXTnoheader}
\alias{xrd.read.TXTnoheader}
\title{Reads the TXT Rigaku XRD file with no header}
\usage{
xrd.read.TXTnoheader(filename)
}
\arguments{
\item{filename}{filename including path}
}
\value{
data frame with XRD data and columns TwoTheta and I
}
\description{
Reads the TXT Rigaku XRD file with no header
}
\examples{

filename = system.file("extdata", "MnPcTheta.txt", package='rigakuXRD')
d = xrd.read.TXTnoheader(filename)

}
