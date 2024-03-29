% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xrd.importData.R
\name{xrd.read.ASC}
\alias{xrd.read.ASC}
\title{Reads the ASC Rigaku XRD file with a Header}
\usage{
xrd.read.ASC(filename)
}
\arguments{
\item{filename}{filename including path}
}
\value{
data frame with XRD data
}
\description{
Reads the ASC Rigaku XRD file with a Header
}
\examples{

fname = xrd.getSampleFiles('asc')[1]
d = xrd.read.ASC(file.path(fname))
plot(d$theta,d$I,log='y',col='red')

}
