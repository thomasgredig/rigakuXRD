#' Estimate crystallite size using the Debye-Scherrer equation
#'
#' @description
#' Computes the crystallite size (or film thickness proxy) from peak
#' statistics using the Debye-Scherrer equation. The result is returned
#' in A (Angstrom).
#'
#' @param peak_stats Numeric vector of peak statistics as returned by
#'   \code{\link{xrd_peak_stats}} (or the legacy \code{\link{xrd.get.PeakStats}}).
#'   The function expects \code{peak_stats[3]} to be the peak position (2theta, degrees)
#'   and \code{peak_stats[4]} to be the Gaussian width parameter \eqn{\sigma}
#'   (degrees), consistent with your existing convention.
#' @param lambda Numeric scalar giving the X-ray wavelength (A). Defaults
#'   to Cu Kalpha radiation (\eqn{1.5406 \AA}).
#' @param K Numeric scalar giving the shape factor (typically 0.9 - 1.0).
#'
#' @details
#' For a Gaussian peak, the full width at half maximum (FWHM) is
#' \eqn{\mathrm{FWHM} = 2.355\,\sigma}. The Scherrer broadening \eqn{\beta}
#' is taken as the FWHM in radians. The Debye - Scherrer estimate is then:
#' \deqn{D = \frac{K \lambda}{\beta \cos(\theta)}}
#' where \eqn{\theta} is the Bragg angle (half of 2theta).
#'
#' @return Numeric scalar giving the estimated size in Angstrom
#'
#' @seealso
#' \code{\link{xrd_peak_stats}} to compute peak statistics used as input.
#'
#' @examples
#' filename <- xrd_sample_files("asc")
#' d <- xrd_import(filename, xrd = TRUE)
#' ds <- xrd_peak_stats(d, peakPos = 38.2)
#' xrd_DebyeScherrer(ds)
#'
#' @export
xrd_DebyeScherrer <- function(peak_stats,
                              lambda = 1.5406,
                              K = 0.9) {
  check_peak_stats(peak_stats)

  if (!is.numeric(lambda) || length(lambda) != 1L || !is.finite(lambda) || lambda <= 0) {
    stop("`lambda` must be a single positive numeric value (Angstrom).")
  }
  if (!is.numeric(K) || length(K) != 1L || !is.finite(K) || K <= 0) {
    stop("`K` must be a single positive numeric value.")
  }

  # Expect: peak position at index 3 (2theta, degrees) and sigma at index 4 (degrees)
  two_theta_deg <- peak_stats[3]
  sigma_deg <- peak_stats[4]

  if (!is.finite(two_theta_deg) || !is.finite(sigma_deg) || sigma_deg <= 0) {
    stop("`peak_stats[3]` (2theta) must be finite and `peak_stats[4]` (sigma) must be a positive finite value.")
  }

  # Convert Gaussian sigma -> FWHM (degrees), then to radians
  beta_rad <- (2.355 * sigma_deg) * pi / 180

  # Bragg angle th = (2th)/2, converted to radians
  theta_rad <- (two_theta_deg / 2) * pi / 180

  K * lambda / (beta_rad * cos(theta_rad))
}



#' @rdname xrd_DebyeScherrer
#'
#' @param PeakStats Legacy name for \code{peak_stats}.
#' @param Lambda Legacy name for \code{lambda}.
#'
#' @export
xrd.get.DebyeScherrer <- function(PeakStats,
                                  Lambda = 1.5406,
                                  K = 0.9) {
  .Deprecated("xrd_DebyeScherrer", package = "rigakuXRD")
  xrd_DebyeScherrer(peak_stats = PeakStats, lambda = Lambda, K = K)
}

