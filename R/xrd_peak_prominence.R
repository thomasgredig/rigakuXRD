#' Compute peak prominence from peak statistics
#'
#' @description
#' Computes the prominence of a fitted peak as the peak amplitude relative to
#' the fitted background, expressed as a percentage.
#'
#' @param peak_stats Numeric vector of peak statistics as returned by
#'   \code{\link{xrd_peak_stats}}.
#'   The function expects \code{peak_stats[1]} to be the background (\code{b})
#'   and \code{peak_stats[2]} to be the amplitude (\code{A}).
#'
#' @return Numeric scalar giving the prominence in percent (\code{100 * A / b}),
#'   rounded to three significant figures.
#'
#' @examples
#' filename <- xrd_sample_files("asc")
#' d <- xrd_import(filename)
#' q <- xrd_peak_stats(d$TwoTheta, d$I, peakPos = 38.2)
#' xrd_peak_prominence(q)
#'
#' @seealso
#' \code{\link{xrd_peak_stats}} to compute the peak statistics used as input.
#'
#' @export
xrd_peak_prominence <- function(peak_stats) {
  check_peak_stats(peak_stats)

  b <- peak_stats[1]
  A <- peak_stats[2]

  if (!is.finite(b) || b <= 0) return(NA_real_)
  if (!is.finite(A)) return(NA_real_)

  signif(A / b * 100, 3)
}

#' @rdname xrd_peak_prominence
#'
#' @param PeakStats Legacy name for \code{peak_stats}.
#'
#' @export
xrd.get.PeakProminence <- function(PeakStats) {
  .Deprecated("xrd_peak_prominence", package = "rigakuXRD")
  xrd_peak_prominence(peak_stats = PeakStats)
}

#' Check that peak statistics are well-formed
#' @noRd
check_peak_stats <- function(PeakStats) {
  if (!is.numeric(PeakStats)) {
    cli_warn("`PeakStats` must be numeric. Generate it with `xrd_peak_stats()`.")
    return(invisible(FALSE))
  }
  if (length(PeakStats) != 8L) {
    cli_warn("`PeakStats` must have length 8. Generate it with `xrd_peak_stats()`.")
    return(invisible(FALSE))
  }
  invisible(TRUE)
}
