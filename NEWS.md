# rigakuXRD 0.5.2

* remove stringr and tools package dependency

# rigakuXRD 0.5.1

* update documentation for functions and readme file
* add return values for all functions
* add more examples

# rigakuXRD 0.5.0

* major fix to RASX import to account for attenuation
* add measurement conditions from RASX file to properly scale intensities

# rigakuXRD 0.4.1

* update documentation for functions
* standardize function names with lower snake case naming convention
* more tests

# rigakuXRD 0.4.0

* clean up documentation, functions
* allow import to create either data frame or XRD class
* use `options(verbose = TRUE)` for functions to return verbose output

# rigakuXRD 0.3.1

* minor code updates to make CRAN compatible, add comments
* adding more examples
* spell out abbreviations and remove local file writing

# rigakuXRD 0.3.0

* cleaned up version
* fixed `xrd.FringeAnalysis` computation

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
