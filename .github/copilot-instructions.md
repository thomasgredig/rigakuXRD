# Copilot instructions for rigakuXRD

## Build, test, and documentation commands

This is an R package. Run commands from the repository root.

```sh
# Run the full testthat suite
Rscript -e 'testthat::test_local()'

# Run one test file
Rscript -e 'testthat::test_file("tests/testthat/test-xrd.R")'

# Run selected tests by name within one file
Rscript -e 'testthat::test_file("tests/testthat/test-findPeak.R", filter = "Peak Position")'

# Regenerate roxygen documentation and NAMESPACE
Rscript -e 'roxygen2::roxygenise()'

# Build and check the package
R CMD build .
R CMD check rigakuXRD_*.tar.gz
```

There is no repository-specific lint configuration or lint command. Keep roxygen comments next to the functions they document; `NAMESPACE` and `man/` are generated outputs and should be regenerated when exported APIs or documentation change.

## Architecture

- `R/xrd_import.R` is the file-format boundary. `xrd_import()` checks the path, dispatches on the lower-case extension (`asc`, `ras`, `txt`, or `rasx`), and returns a data frame by default or assigns class `xrd` when `xrd = TRUE`. The readers normalize their output to `TwoTheta`, `I`, `I.meas`, and `loop`.
- `R/xrd_object.R` defines the lightweight `xrd` S3 representation and its `print`, `plot`, and `as.data.frame` methods. `xrd_filter()` preserves the `xrd` class after filtering. `R/xrd_check.R` contains the shared coercion path used by analysis functions for `xrd` objects, data frames, and paired vectors.
- Peak analysis is layered: `xrd_peak_estimate()` finds approximate dominant-peak parameters; `xrd_find_peak()` refines a position with a Gaussian `stats::nls` fit; `xrd_peak_stats()` returns fitted parameters and standard errors; `xrd_peak_prominence()` and `xrd_DebyeScherrer()` consume those statistics. `xrd_peak_all()` scans interleaved angular grids and repeatedly calls the refinement layer.
- Fringe analysis (`xrd_fringe_analysis()` and `xrd_fringe_thickness()`) operates on imported/filtered spectra, detects fringe positions with LOESS smoothing, and derives thickness from adjacent peak spacing. `export2genx()` reuses the common data/filtering representation to write normalized two-column output.
- `inst/extdata/` contains the four sample ASC, TXT, RAS, and RASX files used by examples and tests. Use `xrd_sample_files()` or `system.file("extdata", ..., package = "rigakuXRD")`; do not hard-code local fixture paths.

## Codebase-specific conventions

- Prefer the current snake_case public API (`xrd_import`, `xrd_find_peak`, `xrd_peak_stats`, etc.). Older dotted/capitalized names remain exported as thin `.Deprecated()` wrappers for compatibility; preserve that pattern when renaming an API rather than deleting the alias.
- Treat `TwoTheta` as the 2theta angle in degrees and `I` as normalized counts-per-second intensity. Preserve `I.meas` (raw/measured intensity) and `loop` when constructing or filtering imported data.
- Analysis functions should accept the existing input forms through `check_dataXRD()`: an `xrd` object, a data frame with `TwoTheta`/`I`, or paired numeric vectors. Preserve this shared normalization instead of duplicating coercion logic.
- Gaussian peak statistics use the positional convention `b`, `A`, `th1`, `sigma`, followed by their four standard errors. Consumers such as `xrd_DebyeScherrer()` and `xrd_peak_prominence()` rely on those positions, including the interpretation of `sigma` in degrees.
- Use the package’s existing `cli_abort()`, `cli_warn()`, and `cli_inform()` imports for user-facing validation/messages where the surrounding code does so. Verbose diagnostics are gated by `options(verbose = TRUE)` through `xrd_inform()`.
- Keep numerical behavior and units explicit: angles are degrees at the public API, wavelength and calculated sizes are in Angstroms unless a function/documentation states otherwise, and fringe-analysis thickness is additionally reported in nm.
- Examples and tests intentionally exercise real Rigaku fixture formats and numerical tolerances. When changing an importer or fitting algorithm, update the relevant fixture-based tests rather than replacing them with synthetic-only coverage.
- `R/_main.R` is a developer scratch script containing a machine-specific absolute path; it is not a package entry point and should not be used as a portable example or test harness.
