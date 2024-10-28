# rigakuXRD

<!-- badges: start -->

[![CRAN status](https://www.r-pkg.org/badges/version/rigakuXRD)](https://CRAN.R-project.org/package=rigakuXRD)

<!-- badges: end -->

The goal of the rigakuXRD package is to import and analyze x-ray diffraction data from the Rigaku Smartlab x-ray diffractometer; mostly for data sets with low-angle reflectivity data or [Bragg-Brentano](https://en.wikipedia.org/wiki/Powder_diffraction) configuration. X-ray diffraction data contains a spectrum with angles and intensities. This package extracts the spectral data from different types of files and provides tools to visualize and analyze that data.

# Installation

You can install the latest version of rigakuXRD from [GitHub](https://github.com/thomasgredig/rigakuXRD) with:

``` r
# install.packages("devtools")
devtools::install_github("thomasgredig/rigakuXRD")
```

# Import XRD Data

The package comes with several sample data files, which can be found with the function `xrd.getSampleFiles()`. X-ray data can be imported from several data formats using `xrd.import()`, which has a filename that includes a path as the argument. A general example of loading a sample x-ray diffraction (XRD) file:

``` r
library(rigakuXRD)
dXRD = xrd.import(filename = xrd.getSampleFiles('asc'), dataXRD = TRUE)
```

If `dataXRD` is TRUE, then an `xrd S3 object` is returned, otherwise a data frame with the same data. In the case, of the xrd S3 object, the data can be easily graphed:

``` r
plot(dXRD)
```

# Peak Finder

The largest peak in a sub data set is found with the `xrd.peakEstimate()` function. It is not a fit, but an estimate to find the parameters of the largest peak in the data set.

Finding the precise peak position, you need to provide a starting angle (see `xrd.peakEstimate()`). The `xrd.find.Peak()` function attempts to fit a Gaussian function to the main peak. Here is an example:

``` r
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
dXRD_30 = xrd_filter(dXRD, 35,40)
peak.stats = xrd.get.PeakStats(dXRD_30, peakPos = 38.217)
peak.prom = xrd.get.PeakProminence(peak.stats)
paste0("Peak prominence: ",signif(peak.prom,3),"%.")
```

You can also get the thickness or grain size using the Debye-Scherrer analysis. The Debye-Scherrer size may be related to the thickness of the film, given that the film is thin enough.

``` r
peak.d = xrd.get.DebyeScherrer(peak.stats)
cat("Debye-Scherrer size from peak:", signif(peak.d/10,4), "nm")
```

## xrd S3 class

The `xrd S3 class` contains x-ray diffraction spectrum data with a data frame that has the following columns:

-   **TwoTheta**: Bragg angle (two theta)

-   **I**: intensity (counts per seconds)

-   **I.meas**: measured intensity

-   **loop**: separate multiple measurements with a number
