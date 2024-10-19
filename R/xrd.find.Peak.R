#' Finds Peak Position from Gaussian
#'
#' Fits a Gaussian form to a peak
#'
#' @param TwoTheta angle
#' @param Intensity intensity signal
#' @param peakPos approximate angle of peak position
#' @param Try.Sigma vector with peak widths used to start fitting
#' @param thetaDelta width of angle to search peak
#' @param verbose logical, if \code{TRUE} provides extra information
#'
#' @return fit peak position
#' @examples
#' filename = xrd.getSampleFiles(fileExt='asc')
#' d = xrd.import(filename)
#' peak.pos = xrd.find.Peak(d$theta, d$I, 38.2)
#' plot(d$theta,d$I,log='y',col='red')
#' abline(v=peak.pos,col='blue')
#'
#' @importFrom stats nls
#' @importFrom dplyr filter
#' @export
xrd.find.Peak <- function(TwoTheta, Intensity,
                          peakPos = NA,
                          Try.Sigma = c(0.1,0.4,0.2,0.15),
                          thetaDelta = 5,
                          verbose=FALSE
                          ) {
  d = data.frame(TwoTheta, I = Intensity)
  if (!is.na(peakPos)) { d <- d %>%
      dplyr::filter(TwoTheta > peakPos - thetaDelta/2 &
               TwoTheta < peakPos + thetaDelta/2) }

  p = xrd.peakEstimate(d$TwoTheta, d$I, verbose)
  if (sd(d$I) > p$A0) { warning("Amplitude drowned in noise."); return(NA) }
  if (is.na(p$s0)) { warning("No Gaussian width."); return(NA) }

  # if peak is partial, it appears all the way on the left or right
  if (min(d$TwoTheta) == p$th0) { warning("Peak leftwards."); return(NA) }
  if (max(d$TwoTheta) == p$th0) { warning("Peak rightwards."); return(NA) }
  if (p$s0>(thetaDelta/4)) { warning("Gaussian width too large, increase thetaDelta."); return(NA) }

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
