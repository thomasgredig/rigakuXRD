#' Fits a Gaussian Function to XRD data
#'
#' @description
#' usually run after a peak is
#' identified through its location
#'
#' @author Thomas Gredig
#'
#' @param TwoTheta 2q angle
#' @param Intensity XRD intensity
#' @param peakPos specific 2q angle of peak position
#' @param Try.Sigma vector with peak widths used to start fitting
#' @param verbose logical, if \code{TRUE} output additional information
#'
#' @return background, amplitude, position, width + 4 std. errors
#' @examples
#' filename = xrd.getSampleFiles(fileExt='asc')
#' d = xrd.read.ASC(filename)
#' xrd.get.PeakStats(d$theta, d$I, 38.2)
#'
#' @importFrom stats nls sd predict
#' @importFrom graphics lines
#' @export
xrd.get.PeakStats <- function(TwoTheta, Intensity, peakPos,
                              Try.Sigma = c(0.2,0.1,0.05,0.3),
                              verbose = FALSE) {
  d = data.frame(TwoTheta, I = Intensity)
  for(pw in Try.Sigma) {

    deltaPeak = max(0.2, 4*pw)
    fit <- NULL
    n1 = subset(d, TwoTheta > (peakPos-deltaPeak) &
                  TwoTheta < (peakPos+deltaPeak))
    if (verbose) plot(n1)
    if (verbose) print(paste("Fit from 2q =",
                             min(n1$TwoTheta), "to",
                             max(n1$TwoTheta)))
    if(nrow(n1)>5) {
      q = xrd.peakEstimate(n1$TwoTheta, n1$I)
      background = ceiling(q$b0)
      A1 = q$A0
      p1 = q$th0
      peak.width = pw

      if (verbose) {
        print(paste("background=",background,
                    " A=",A1,
                    " t0=",p1,
                    " sigma=",peak.width))
      }
      # if ((min(n1$TwoTheta)==p1) | (min(n1$TwoTheta)==p1)) break;
      # if (sd(n1$I)*4 > A1) break;
      try(fit <-
            nls(data = n1,
                I ~ b + A*exp(-(TwoTheta-th1)^2/(2*sigma*sigma)),
                start = list(b=background, A = A1, th1=p1, sigma=pw))
      ); # does not stop in case of error

      if (!is.null(fit)) break;
    }
  }
  if (verbose) {
    if (!is.null(fit)) {
      plot(n1)
      lines(n1$TwoTheta, predict(fit),col='red')
    }
  }

  if(is.null(fit)) { return(NA) }
  summary(fit)$coeff[1:8]
}
