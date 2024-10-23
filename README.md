# rigakuXRD

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/rigakuXRD)](https://CRAN.R-project.org/package=rigakuXRD)

<!-- badges: end -->

The goal of the rigakuXRD package is to analyze x-ray diffraction data from the Rigaku Smartlab x-ray diffractometer; mostly for data sets with low-angle reflectivity data or [Bragg-Brentano](https://en.wikipedia.org/wiki/Powder_diffraction) configuration. X-ray diffraction data contains a spectrum with angles and intensities. This package extracts the spectral data from different types of files and provides tools to visualize and analyze that data.

# Installation

You can install the released version of rigakuXRD from [GitHub](https://github.com/thomasgredig/rigakuXRD) with:

``` r
# install.packages("devtools")
devtools::install_github("thomasgredig/rigakuXRD")
```

# Import XRD Data

The package comes with several sample data files, which can be found with the function `xrd.getSampleFiles()`. A general example of loading a sample x-ray diffraction (XRD) file:

``` r
library(rigakuXRD)
d = xrd.import(filename = xrd.getSampleFiles()[1])
```

The data can be graphed, typically on a semi-log graph:

``` r
plot(d$theta, d$I.meas, log='y')
```

If you are loading a text file without a header, you could use the following approach:

``` r
d = xrd.read.TXTnoheader('xrd.txt')
plot(d$theta, d$I.meas, log='y')
```

## Importing RAS or RASX Files

In addition to `.asc` files, `.ras` or `.rasx` files are also text files and contain a header information, followed by the data. Here is an example:

``` r
fileName = xrd.getSampleFiles(fileExt = 'rasx')[1]
d = xrd.import(fileName)
```

# Peak Analsysis

The largest peak in a sub dataset is found with the `xrd.peakEstimate()` function. It is not a fit, but an estimate to find the parameters of the largest peak in the data set.

Finding the precise peak position, you need to provide a starting angle (see `xrd.peakEstimate()`). The `xrd.find.Peak()` function attempts to fit a Gaussian function to the main peak. Here is an example:

``` r
fileName = xrd.getSampleFiles(fileExt = 'asc')[1]
d = xrd.import(fileName)

library(dplyr)
d %>% filter(theta < 42 & theta > 32) -> d1
p = xrd.peakEstimate(d1$theta, d1$I)

# is there a Au peak ?
xrd.find.Peak(d$theta, d$I, p[3])

# is there a Si substrate peak ?
xrd.find.Peak(d$theta, d$I, 44, Try.Sigma = c(0.05,0.1) )
```

You can search for all the prominent peaks, using the following function, which returns a vector with all the peaks.

``` r
xrd.get.AllPeaks(d$theta, d$I)
```

## Peak Analysis

After finding the peaks, the peak amplitude is compared to the background to check the prominence of the peak, it should rise above 5%:

``` r
peak.stats = xrd.get.PeakStats(d$theta, d$I, 38.217)
peak.prom = xrd.get.PeakProminence(peak.stats)
paste0("Peak prominence: ",signif(peak.prom,3),"%.")
```

You can also get the thickness or grain size using the Debye-Scherrer analysis.

``` r
peak.d = xrd.get.DebyeScherrer(peak.stats)
paste("Debye-Scherrer thickness from peak:", peak.d/10, "nm")
```
