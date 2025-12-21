#' Find peaks in an XRD spectrum
#'
#' @description
#' Scans an XRD spectrum for candidate peak locations and refines each
#' candidate via \code{\link{xrd_find_peak}} in a local window. Results are
#' returned as unique peak positions rounded to 0.01 degrees.
#'
#' @param data A data frame or \code{xrd} object containing \code{TwoTheta}
#'   and intensity (\code{I}) data.
#' @param min_prominence Numeric scalar. Reserved for future use (currently
#'   not applied).
#' @param try_sigma Numeric vector of starting guesses for Gaussian width
#'   (\eqn{\sigma}) passed to \code{\link{xrd_find_peak}}.
#' @param delta_theta Numeric scalar giving the half-width (in degrees) of
#'   the local 2θ window used for fitting around each candidate.
#' @param range Numeric length-2 vector giving the 2θ interval to search
#'   (degrees), e.g. \code{c(20, 80)}.
#' @param step_size Numeric scalar giving the spacing (degrees) between
#'   candidate peak locations used in the scan.
#'
#' @details
#' Candidate locations are generated on two interleaved grids separated by
#' \code{step_size/2}, which helps reduce sensitivity to grid alignment. Each
#' candidate is refined using \code{\link{xrd_find_peak}} on a subset of the
#' data defined by \code{delta_theta}. Returned peaks are deduplicated and
#' rounded to 0.01 degrees.
#'
#' @return Numeric vector of peak positions (2θ, degrees), sorted and unique,
#'   rounded to 0.01 degrees. Returns \code{numeric(0)} if no peaks are found.
#'
#' @seealso
#' \code{\link{xrd_find_peak}} for refining a single peak location;
#' \code{\link{xrd_filter}} for restricting an XRD spectrum to a 2θ range.
#'
#' @examples
#' filename <- xrd_sample_files("asc")
#' d <- xrd_import(filename)
#' xrd_peak_all(d, delta_theta = 2, try_sigma = c(0.1), range = c(42, 46))
#'
#' @export
xrd_peak_all <- function(data,
                         min_prominence = 5,
                         try_sigma = c(0.2, 0.1, 0.05, 0.3),
                         delta_theta = 5,
                         range = c(0, 90),
                         step_size = 2) {
  # Normalize/validate input
  data_xrd <- check_dataXRD(data)

  if (!is.numeric(range) || length(range) != 2L || any(!is.finite(range))) {
    stop("`range` must be a finite numeric vector of length 2, e.g. c(0, 90).")
  }
  range <- sort(range)

  if (!is.numeric(delta_theta) || length(delta_theta) != 1L || !is.finite(delta_theta) || delta_theta <= 0) {
    stop("`delta_theta` must be a single positive numeric value.")
  }

  if (!is.numeric(step_size) || length(step_size) != 1L || !is.finite(step_size) || step_size <= 0) {
    stop("`step_size` must be a single positive numeric value.")
  }

  # NOTE: min_prominence is currently not applied; keep for API stability.
  # (If you later implement it, do so after peak refinement.)
  invisible(min_prominence)

  # Filter to requested search interval
  d <- xrd_filter(data_xrd, range[1], range[2])
  if (is.null(d) || nrow(d) < 3L) return(numeric(0))

  theta_min <- min(d$TwoTheta, na.rm = TRUE) + step_size / 2
  theta_max <- max(d$TwoTheta, na.rm = TRUE) - step_size / 2

  if (!is.finite(theta_min) || !is.finite(theta_max) || (theta_max - theta_min) < step_size) {
    warning("XRD data has insufficient angular range in the requested `range`.")
    return(numeric(0))
  }

  # Interleaved candidate grids
  cand1 <- seq(from = theta_min, to = theta_max, by = step_size)
  cand2 <- seq(from = theta_min + step_size / 2, to = theta_max - step_size / 2, by = step_size)
  candidates <- sort(unique(c(cand1, cand2)))

  peaks <- numeric(0)

  for (ang in candidates) {
    window <- xrd_filter(d, ang - delta_theta, ang + delta_theta)
    if (is.null(window) || nrow(window) < 3L) next

    p <- xrd_find_peak(
      TwoTheta = window$TwoTheta,
      Intensity = window$I,
      peakPos = ang,
      Try.Sigma = try_sigma
    )

    if (is.numeric(p) && length(p) == 1L && is.finite(p)) {
      peaks <- c(peaks, p)
    }
  }

  sort(unique(round(peaks, 2)))
}




#' @rdname xrd_peak_all
#'
#' @param min.Prominence Legacy name for \code{min_prominence}.
#' @param Try.Sigma Legacy name for \code{try_sigma}.
#' @param deltaTheta Legacy name for \code{delta_theta}.
#' @param Range Legacy name for \code{range}.
#'
#' @export
xrd.get.AllPeaks <- function(data,
                             min.Prominence = 5,
                             Try.Sigma = c(0.2, 0.1, 0.05, 0.3),
                             deltaTheta = 5,
                             Range = c(0, 90)) {
  .Deprecated("xrd_peak_all", package = "rigakuXRD")
  xrd_peak_all(
    data = data,
    min_prominence = min.Prominence,
    try_sigma = Try.Sigma,
    delta_theta = deltaTheta,
    range = Range
  )
}
