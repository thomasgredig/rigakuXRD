#' Estimate dominant peak parameters from an XRD spectrum
#'
#' @description
#' Provides a simple, data-driven estimate of the most intense peak in a
#' selected 2θ range. This function does not perform a nonlinear fit; it
#' identifies the maximum intensity after a coarse interpolation and returns
#' approximate peak parameters: background (\code{b0}), amplitude (\code{A0}),
#' peak position (\code{th0}), and an estimated Gaussian width parameter
#' (\code{s0}).
#'
#' @param TwoTheta Numeric vector of 2θ angles from an XRD spectrum, or an
#'   \code{xrd} S3 object.
#' @param Intensity Numeric vector of intensities corresponding to
#'   \code{TwoTheta}. If \code{NULL}, \code{TwoTheta} is assumed to contain the
#'   intensity data (e.g., via an \code{xrd} object).
#'
#' @details
#' The spectrum is interpolated to a small number of points (default: 20) to
#' reduce sensitivity to high-frequency noise. The dominant peak is taken as
#' the maximum of the interpolated intensities. The width estimate \code{s0}
#' is derived from a point to the right of the peak; if an estimate cannot be
#' computed robustly (e.g., peak too close to the boundary), \code{s0} is
#' returned as \code{NA_real_}.
#'
#' @return A named list with the following elements:
#' \describe{
#'   \item{\code{b0}}{Estimated background intensity (minimum of interpolated data).}
#'   \item{\code{A0}}{Estimated peak amplitude above background.}
#'   \item{\code{th0}}{Estimated peak position (2θ, in degrees).}
#'   \item{\code{s0}}{Estimated Gaussian width parameter (half-width proxy); may be \code{NA_real_}.}
#' }
#'
#' @examples
#' d <- xrd_import(xrd_sample_files("asc"), xrd = TRUE)
#' d <- xrd_filter(d, 30, 42)
#' p <- xrd_peak_estimate(d)
#' plot(d); abline(v = p$th0, lwd = 2)
#' abline(h = p$b0, col = "darkgrey")
#' p
#'
#' @importFrom stats approx
#' @export
xrd_peak_estimate <- function(TwoTheta, Intensity = NULL) {
  dXRD <- check_dataXRD(TwoTheta, Intensity)

  tt <- dXRD$TwoTheta
  I  <- dXRD$I

  # Coarse interpolation to reduce sensitivity to noise
  q <- approx(tt, I, n = 20)
  if (length(q$x) < 3L || all(!is.finite(q$y))) {
    return(list(b0 = NA_real_, A0 = NA_real_, th0 = NA_real_, s0 = NA_real_))
  }

  # Dominant peak
  peakNo <- which.max(q$y)
  th0 <- q$x[peakNo]

  # Background and amplitude
  b0 <- min(q$y, na.rm = TRUE)
  A0 <- max(q$y, na.rm = TRUE) - b0

  # Width estimate (robust to boundary conditions / degenerate cases)
  s0 <- NA_real_
  idx <- peakNo + 2L
  if (is.finite(A0) && A0 > 0 && idx <= length(q$y)) {
    ts0 <- (q$y[idx] - b0) / A0

    # Ensure the log argument is valid and non-degenerate
    if (is.finite(ts0) && ts0 > 0 && ts0 < 1) {
      dx <- q$x[idx] - th0
      if (is.finite(dx) && dx != 0) {
        s0 <- -dx / (sqrt(2) * log(ts0))
      }
    }
  }

  list(b0 = b0, A0 = A0, th0 = th0, s0 = s0)
}

#' @rdname xrd_peak_estimate
#' @export
xrd.peakEstimate <- function(TwoTheta, Intensity = NULL) {
  .Deprecated("xrd_peak_estimate", package = "rigakuXRD")
  xrd_peak_estimate(TwoTheta = TwoTheta, Intensity = Intensity)
}
