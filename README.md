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

## Example

This is a basic example which shows you how to analyze the XRD data:

``` r
library(rigakuXRD)
## basic example code
fn = system.file("extdata", "2Theta.asc", package='rigakuXRD')
library(stringr)
d <- xrd.read.ASC(fn)
plot(d$theta, d$I.meas, log='y')
```

Finding the peak position

```r
# is there a Au peak ?
xrd.find.Peak(d$theta, d$I, 38)
# is there a Si substrate peak ?
xrd.find.Peak(d$theta, d$I, 44)
```

Finding all the peaks:

```r
p=c()
for(ang in 10:50) {
  a1 = xrd.find.Peak(d$theta, d$I, ang)
  if(!is.na(a1)) {
    p = c(p,a1)
  }
}
```

After finding the peaks, the peak amplitude is compared to the background to check the prominence of the peak, it should rise above 5%:

```r
q = xrd.get.PeakStats(d$theta, d$I, 38.217)
paste0("Peak prominence: ",signif(q[2]/q[1]*100,3),"%.")
```

