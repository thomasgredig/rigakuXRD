#' Fit a Gaussian peak model and return peak statistics
#'
#' @description
#' Fits a Gaussian model to an XRD peak near \code{peakPos} to estimate the
#' background, amplitude, peak position, and width (sigma), along with their
#' standard errors.
#'
#' @param TwoTheta Numeric vector of 2θ angles from an XRD spectrum, or an
#'   \code{xrd} S3 object.
#' @param Intensity Numeric vector of intensities corresponding to
#'   \code{TwoTheta}. If \code{NULL}, \code{TwoTheta} is assumed to contain the
#'   intensity data (e.g., via an \code{xrd} object).
#' @param peakPos Numeric scalar giving an approximate peak position (2θ, degrees).
#' @param try_sigma Numeric vector of initial guesses for the Gaussian width
#'   parameter \eqn{\sigma} (degrees) used to initialize the nonlinear fit.
#'
#' @details
#' For each starting value in \code{try_sigma}, the function subsets the data to a
#' local window around \code{peakPos} (with half-width \code{max(0.2, 4*sigma)}),
#' estimates initial parameters using \code{\link{xrd_peak_estimate}}, and then
#' attempts a nonlinear least-squares fit via \code{\link[stats]{nls}}.
#'
#' @return
#' A numeric vector of length 8 containing the parameter estimates and their
#' standard errors in the following order:
#' \describe{
#'   \item{\code{b}}{Background estimate.}
#'   \item{\code{A}}{Amplitude estimate.}
#'   \item{\code{th1}}{Peak position (2θ, degrees).}
#'   \item{\code{sigma}}{Gaussian width parameter \eqn{\sigma} (degrees).}
#'   \item{\code{b.se}}{Standard error of \code{b}.}
#'   \item{\code{A.se}}{Standard error of \code{A}.}
#'   \item{\code{th1.se}}{Standard error of \code{th1}.}
#'   \item{\code{sigma.se}}{Standard error of \code{sigma}.}
#' }
#' Returns \code{NA} if all fit attempts fail.
#'
#' @examples
#' filename <- xrd_sample_files(fileExt = "asc")
#' d <- xrd_import(filename)
#' xrd_peak_stats(d$TwoTheta, d$I, peakPos = 38.2)
#'
#' @importFrom stats nls
#' @export
xrd_peak_stats <- function(TwoTheta,
                           Intensity = NULL,
                           peakPos,
                           try_sigma = c(0.2, 0.1, 0.05, 0.3)) {
  d <- check_dataXRD(TwoTheta, Intensity)

  fit <- NULL

  for (pw in try_sigma) {
    delta_peak <- max(0.2, 4 * pw)

    n1 <- subset(
      d,
      TwoTheta > (peakPos - delta_peak) & TwoTheta < (peakPos + delta_peak)
    )

    if (nrow(n1) <= 5L) next

    q <- xrd_peak_estimate(n1$TwoTheta, n1$I)
    background <- ceiling(q$b0)
    A1 <- q$A0
    p1 <- q$th0

    # Attempt fit; do not stop on error
    fit_try <- try(
      nls(
        data = n1,
        I ~ b + A * exp(-(TwoTheta - th1)^2 / (2 * sigma * sigma)),
        start = list(b = background, A = A1, th1 = p1, sigma = pw)
      ),
      silent = TRUE
    )

    if (!inherits(fit_try, "try-error")) {
      fit <- fit_try
      break
    }
  }

  if (is.null(fit)) return(NA)

  cf <- summary(fit)$coeff
  out <- c(
    b = cf["b", "Estimate"],
    A = cf["A", "Estimate"],
    th1 = cf["th1", "Estimate"],
    sigma = cf["sigma", "Estimate"],
    b.se = cf["b", "Std. Error"],
    A.se = cf["A", "Std. Error"],
    th1.se = cf["th1", "Std. Error"],
    sigma.se = cf["sigma", "Std. Error"]
  )

  out
}

#' @rdname xrd_peak_stats
#'
#' @param Try.Sigma Legacy name for \code{try_sigma}.
#'
#' @export
xrd.get.PeakStats <- function(TwoTheta,
                              Intensity = NULL,
                              peakPos,
                              Try.Sigma = c(0.2, 0.1, 0.05, 0.3)) {
  .Deprecated("xrd_peak_stats", package = "rigakuXRD")
  xrd_peak_stats(
    TwoTheta = TwoTheta,
    Intensity = Intensity,
    peakPos = peakPos,
    try_sigma = Try.Sigma
  )
}
