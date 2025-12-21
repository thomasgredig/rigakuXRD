#' Determine peak position via Gaussian fit
#'
#' Fits a Gaussian model to a diffraction peak in an XRD spectrum and
#' returns the refined peak position. The fit is performed locally
#' around an approximate peak position.
#'
#' @param TwoTheta Numeric vector of 2θ angles from an XRD spectrum.
#' @param Intensity Numeric vector of intensities corresponding to
#'   \code{TwoTheta}. If \code{NULL}, \code{TwoTheta} is assumed to be
#'   part of an XRD S3 object.
#' @param peakPos Numeric scalar giving an approximate 2θ position of
#'   the peak to be fitted.
#' @param Try.Sigma Numeric vector of initial guesses for the Gaussian
#'   peak width (\eqn{\sigma}) used to initialize the nonlinear fit.
#' @param thetaDelta Numeric scalar giving the half-width (in degrees)
#'   of the 2θ window centered on \code{peakPos} used for fitting.
#'
#' @details
#' The function subsets the XRD spectrum to a window defined by
#' \code{peakPos ± thetaDelta} and performs a nonlinear least-squares
#' fit of a Gaussian peak model using \code{\link[stats]{nls}}. Multiple
#' starting values for the peak width are attempted to improve
#' robustness. If the fit fails, a warning is issued.
#'
#' @return Numeric scalar giving the fitted peak position (2θ, in degrees).
#'
#' @examples
#' filename <- xrd_sample_files(fileExt = "asc")
#' d <- xrd.import(filename, xrd = TRUE)
#' peak.pos <- xrd_find_peak(d$TwoTheta, d$I, peakPos = 38.2)
#' plot(d)
#' abline(v = peak.pos, col = "blue")
#'
#' @importFrom stats nls sd
#' @importFrom cli cli_warn
#' @export
xrd_find_peak <- function(TwoTheta,
                          Intensity = NULL,
                          peakPos = NA,
                          Try.Sigma = c(0.1,0.4,0.2,0.15),
                          thetaDelta = 5
                          ) {
  d <- check_dataXRD(TwoTheta, Intensity)

  if (!is.na(peakPos)) d <- xrd_filter(d, peakPos - thetaDelta/2, peakPos + thetaDelta/2)

  p = xrd_peak_estimate(d$TwoTheta, d$I)
  if (sd(d$I) > p$A0) { cli_warn("Amplitude drowned in noise."); return(NA) }
  if (is.na(p$s0)) { cli_warn("No Gaussian width."); return(NA) }

  # if peak is partial, it appears all the way on the left or right
  if (min(d$TwoTheta) == p$th0) { cli_warn("Peak leftwards."); return(NA) }
  if (max(d$TwoTheta) == p$th0) { cli_warn("Peak rightwards."); return(NA) }
  if (p$s0>(thetaDelta/4)) { cli_warn("Gaussian width too large, increase thetaDelta."); return(NA) }

  fit <- NULL

  Try.Sigma = c(Try.Sigma, p$s0)
  for(peak.width in Try.Sigma) {
    fit <- NULL
    try(fit <-
          nls(data = d,
              I ~ b + A*exp(-(TwoTheta-t)^2/(2*s*s)),
              start = list(b=p$b0, A = p$A0,
                           t=p$th0, s=peak.width))
    ); # does not stop in case of error
    if (!is.null(fit)) break;
  }
  if(is.null(fit)) { return(NA) }
  summary(fit)$coef[3]
}


#' @rdname xrd_find_peak
#' @export
xrd.find.Peak <- function(TwoTheta,
                          Intensity = NULL,
                          peakPos = NA,
                          Try.Sigma = c(0.1, 0.4, 0.2, 0.15),
                          thetaDelta = 5) {
  .Deprecated("xrd_find_peak", package = "rigakuXRD")
  xrd_find_peak(
    TwoTheta = TwoTheta,
    Intensity = Intensity,
    peakPos = peakPos,
    Try.Sigma = Try.Sigma,
    thetaDelta = thetaDelta
  )
}

