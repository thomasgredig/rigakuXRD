#' Fits a Gaussian function to XRD data
#'
#' @description
#' Given an XRD peak, a Gaussian fit at the provided location \code{peakPos}
#' a Gaussian fit is made to find the exact location, width, and background.
#'
#' @author Thomas Gredig
#'
#' @param TwoTheta vector of two theta angles from xrd spectrum.
#' @param Intensity \code{NULL} if xrd S3 object, otherwise xrd intensity vector.
#' @param peakPos approximate 2Theta angle of peak position.
#' @param Try.Sigma vector with peak widths used to start fitting.
#'
#' @return background, amplitude, position, width + 4 std. errors from peak fit
#' @examples
#' filename = xrd.getSampleFiles(fileExt='asc')
#' d = xrd.import(filename)
#' xrd.get.PeakStats(d$TwoTheta, d$I, 38.2)
#'
#' @importFrom stats nls sd predict
#' @importFrom graphics lines
#' @export
xrd.get.PeakStats <- function(TwoTheta,
                              Intensity = NULL,
                              peakPos,
                              Try.Sigma = c(0.2,0.1,0.05,0.3)) {
  d <- check_dataXRD(TwoTheta, Intensity)
  for(pw in Try.Sigma) {

    deltaPeak = max(0.2, 4*pw)
    fit <- NULL
    n1 = subset(d, TwoTheta > (peakPos-deltaPeak) &
                  TwoTheta < (peakPos+deltaPeak))

    xrd_inform("Fit from 2theta = {min(n1$TwoTheta)} to {max(n1$TwoTheta)}.")
    if(nrow(n1)>5) {
      q = xrd.peakEstimate(n1$TwoTheta, n1$I)
      background = ceiling(q$b0)
      A1 = q$A0
      p1 = q$th0
      peak.width = pw


      xrd_inform("background= {background}, A= {A1}, t0= {p1}, sigma= {peak.width}")

      try(fit <-
            nls(data = n1,
                I ~ b + A*exp(-(TwoTheta-th1)^2/(2*sigma*sigma)),
                start = list(b=background, A = A1, th1=p1, sigma=pw))
      ); # does not stop in case of error

      if (!is.null(fit)) break
    }
  }

  if(is.null(fit)) { return(NA) }
  summary(fit)$coeff[1:8]
}
