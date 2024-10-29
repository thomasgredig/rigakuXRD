# rigakuXRD

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/rigakuXRD)](https://CRAN.R-project.org/package=rigakuXRD)

<!-- badges: end -->

The goal of the rigakuXRD R package is to import and analyze x-ray diffraction (XRD) and reflectivity (XRR) data from the Rigaku Smartlab x-ray diffractometer; mostly for data sets with low-angle reflectivity data or [Bragg-Brentano](https://en.wikipedia.org/wiki/Powder_diffraction) configuration. X-ray diffraction data contains a spectrum with angles and intensities. This package extracts the spectral data from different types of files and provides tools to visualize and analyze that data.

# Installation

You can install the latest version of the [rigakuXRD R package](https://github.com/thomasgredig/rigakuXRD) from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("thomasgredig/rigakuXRD")
```

# Import XRD Data

The package comes with several sample data files, which can be found with the function `xrd.getSampleFiles()`. The data file extensions are .ASC, .RAS or .RASX. X-ray diffraction data can be imported from different data formats using `xrd.import()`, which uses the file extension to implement the corresponding data loader. An example of loading a sample x-ray diffraction (XRD) file:

``` r
library(rigakuXRD)
filename = xrd.getSampleFiles('asc')
dXRD = xrd.import(filename, xrd = TRUE)
```

If `xrd` is TRUE, then an `xrd S3 object` is returned, otherwise a data frame with the same data. In the case, of the xrd S3 object, it is more convenient to graph the data quickly on a logarithmic scale:

``` r
plot(dXRD)
```

Alternatively, the data frame can also be graphed using the **TwoTheta** and **I** columns.

``` r
df = xrd.import(filename)
plot(df$TwoTheta, df$I, log='y')
```

# Peak Finder

The largest peak in a sub data set is found with the `xrd.peakEstimate()` function. It is not a fit, but an estimate to find the parameters of the largest peak in the data set.

Finding the precise peak position, you need to provide a starting angle (see `xrd.peakEstimate()`). The `xrd.find.Peak()` function attempts to fit a Gaussian function to the main peak. Here is an example:

``` r
# select the data range from 2theta = 10 - 60o
dXRD_60 = xrd_filter(dXRD, 10,60)
p = xrd.peakEstimate(dXRD_60)
# fit the main peak between 10 deg and 60 deg
xrd.find.Peak(dXRD_60, p$th0)
plot(dXRD_60)
abline(v=p$th0)

# find exact position of the peak
dXRD_40 = xrd_filter(dXRD, 40,50)
plot(dXRD_40)
peakPos = xrd.find.Peak(dXRD_40, p$th0)
abline(v=peakPos)
```

You can search for all the prominent peaks, using the following function, which returns a vector with all the peaks.

``` r
xrd.get.AllPeaks(dXRD_60)
```

## Peak Analysis

After finding the peaks, the peak amplitude is compared to the background to check the prominence of the peak, it should rise above 5%:

``` r
dXRD_35 = xrd_filter(dXRD, 35,40)
peak_stats = xrd.get.PeakStats(dXRD_35, peakPos = 38.217)
peak_prom = xrd.get.PeakProminence(peak_stats)
paste0("Peak prominence: ",signif(peak_prom,3),"%.")
```

You can also get the thickness or grain size using the Debye-Scherrer analysis. The Debye-Scherrer size may be related to the thickness of the film, given that the film is thin enough.

``` r
peak_d = xrd.get.DebyeScherrer(peak_stats)
cat("Debye-Scherrer size from peak:", signif(peak_d/10,4), "nm")
```

## xrd object

The `xrd S3 class` contains x-ray diffraction spectrum data with a data frame that has the following columns:

-   **TwoTheta**: Bragg angle (two theta)

-   **I**: intensity (counts per seconds)

-   **I.meas**: measured intensity

-   **loop**: separate multiple measurements with a number

You can convert the object to a data frame use `as.data.frame()` and also `print` the object. Passing an `xrd` object to several other functions, makes it easy to analyze the data.

## Options

For verbose output, set the `verbose` option to TRUE.

``` r
options(verbose=TRUE)
```
