# rigakuXRD 0.3.0

* cleaned up version

# rigakuXRD 0.2.6

* allow space in addition to tab in loading txt XRD files
* add function to export XRD to GenX input format

# rigakuXRD 0.2.5

* geometrical factor in Scherrer equation can be changed, see `xrd.get.Scherrer()`.
* convert RAS text header to unicode

# rigakuXRD 0.2.4

* add the `xrdFindPeaks` vignette to explain how the peaks are found
* update the `xrd.find.Peak()` function using a new way to estimate the peak position with `xrd.peakEstimate()`, which will find the largest peak (not necessarily near the value provided as the estimated peak)

# rigakuXRD 0.2.3

* fix examples
* update RAS file reader
* reading of ASC, TXT, RAS, and RASX files should yield the same columns, when using `xrd.import()`, unify columns

# rigakuXRD 0.2.2

* adding reader for RASX files, which are compresse

# rigakuXRD 0.2.1

* adding reader for ASC header: `xrd.readHeader.ASC()`

# rigakuXRD 0.2.0

* clean up help
* use `xrd.importData` to read XRD data files
* Added a `NEWS.md` file to track changes to the package.
