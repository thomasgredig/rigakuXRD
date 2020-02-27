# rigakuXRD

<!-- badges: start -->
<!-- badges: end -->

The goal of rigakuXRD is to analyze x-ray diffraction data from the Rigaku Smartlab x-ray diffractometer.

## Installation

You can install the released version of rigakuXRD from [GitHub](https://github.com/thomasgredig/rigakuXRD) with:

``` r
# install.packages("devtools")
devtools::install_github("thomasgredig/rigakuXRD")
```

The first line installs the popular `devtools` package that you can use to install R packages from (Github)[http://www.github.com]. The second lines installs the latest version of the `rigakuXRD` package.


## Example for Loading Data

This is a basic example which shows you how to analyze the XRD data:

```r
library(rigakuXRD)
## basic example code
fn = system.file("extdata", "2Theta.asc", package='rigakuXRD')

# load an ASC file
d <- xrd.read.ASC(fn)
plot(d$theta, d$I.meas, log='y')
```

If you are loading a text file wihtout a header:

```r
d = xrd.read.TXTnoheader('xrd.txt')
plot(d$theta, d$I.meas, log='y')
```

## Loading RAS files

In addition to `.ASC` files, `.RAS` files are also text files and contain a header information, followed by the data. Here is an example:

```r
library(rigakuXRD)
library(checkRAWfolder)

file.list = raw.findFile('.',instrument='xrd')
d = xrd.read.RAS(file.list[1])
```

## Example for Finding Peaks

Finding the peak position, you need to provide a starting angle, where you expect to find a peak to find. If no peak is found, it may be that the width is unusual and you can specify the **Try.Sigma** parameter with a vector of expected peak widths at that location.

```r
# is there a Au peak ?
xrd.find.Peak(d$theta, d$I, 38)

# is there a Si substrate peak ?
xrd.find.Peak(d$theta, d$I, 44, Try.Sigma = c(0.05,0.1) )
```

You can search for all the prominent peaks, using the following function, which returns a vector with all the peaks.

```r
xrd.get.AllPeaks(d$theta, d$I)
```


## Example for Analyzing Peaks

After finding the peaks, the peak amplitude is compared to the background to check the prominence of the peak, it should rise above 5%:

```r
peak.stats = xrd.get.PeakStats(d$theta, d$I, 38.217)
peak.prom = xrd.get.PeakProminence(peak.stats)
paste0("Peak prominence: ",signif(peak.prom,3),"%.")
```

You can also get the thickness or grain size using the Debye-Scherrer analysis.

```r
peak.d = xrd.get.DebyeScherrer(peak.stats)
paste("Debye-Scherrer thickness from peak:", peak.d/10, "nm")
```

