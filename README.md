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

The package includes several sample X-ray diffraction (XRD) data files, which can be located using the function `xrd_sample_files()`. These files are provided in the `.ASC`, `.RAS`, and `.RASX` formats. XRD data may be imported from these and other supported formats using `xrd.import()`, which automatically selects the appropriate reader based on the file extension. The following example demonstrates how to load a sample XRD data file:

``` r
library(rigakuXRD)
filename = xrd_sample_files('asc')
dXRD = xrd_import(filename, xrd = TRUE)
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

The largest peak in a sub data set is found with the `xrd_peak_estimate()` function. It is not a fit, but an estimate to find the parameters of the largest peak in the data set.

Finding the precise peak position, you need to provide a starting angle (see `xrd_peak_estimate()`). The `xrd_find_peak()` function attempts to fit a Gaussian function to the main peak. Here is an example:

``` r
# select the data range from 2theta = 10 - 60 deg
dXRD_60 = xrd_filter(dXRD, 10,60)
p = xrd_peak_estimate(dXRD_60)
# fit the main peak between 10 deg and 60 deg
xrd_find_peak(dXRD_60, p$th0)
plot(dXRD_60)
abline(v=p$th0)

# find exact position of the peak
dXRD_40 = xrd_filter(dXRD, 40,50)
plot(dXRD_40)
peakPos = xrd_find_peak(dXRD_40, p$th0)
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
peak_stats = xrd_peak_stats(dXRD_35, peakPos = 38.217)
peak_prom = xrd_peak_prominence(peak_stats)
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
