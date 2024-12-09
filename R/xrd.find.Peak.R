#' Finds peak position from gaussian fit
#'
#' Fits a Gaussian form to a peak
#'
#' @param TwoTheta vector of two theta angles from xrd spectrum.
#' @param Intensity \code{NULL} if xrd S3 object, otherwise xrd intensity vector.
#' @param peakPos approximate angle (2Theta) of peak position.
#' @param Try.Sigma vector with peak widths used to start fitting.
#' @param thetaDelta approximate width of angle to search peak.
#'
#' @return peak position angle obtained from fit
#' @examples
#' filename = xrd.getSampleFiles(fileExt='asc')
#' d = xrd.import(filename)
#' peak.pos = xrd.find.Peak(d$TwoTheta, d$I, 38.2)
#' plot(d$TwoTheta,d$I,log='y',col='red')
#' abline(v=peak.pos,col='blue')
#'
#' @importFrom stats nls
#' @importFrom cli cli_warn
#' @export
xrd.find.Peak <- function(TwoTheta,
                          Intensity = NULL,
                          peakPos = NA,
                          Try.Sigma = c(0.1,0.4,0.2,0.15),
                          thetaDelta = 5
                          ) {
  d <- check_dataXRD(TwoTheta, Intensity)

  if (!is.na(peakPos)) d <- xrd_filter(d, peakPos - thetaDelta/2, peakPos + thetaDelta/2)

  p = xrd.peakEstimate(d$TwoTheta, d$I)
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
