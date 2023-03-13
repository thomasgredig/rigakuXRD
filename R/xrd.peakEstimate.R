#' Estimate Highest Peak in Selected Data Set
#'
#' @description
#' Simple peak position estimate, yields largest peak only
#' in a particular data set. This is not a fit, just an
#' estimate based on the data at hand.
#' It returns the background (b0), the amplitude (A0), the
#' peak position (th0), and the half-width sigma (s0). In
#' order to learn more, turn on the *verbose* output.
#'
#' @param twoTheta XRD 2q angle
#' @param I intensity
#' @param verbose logical, if \code{TRUE}, prints a graph of the data and fit
#'
#' @returns
#' 4 estimated parameters: background (b0), amplitude (A0), peakposition (th0), and half-width (s0)
#'
#' @examples
#' d = xrd.import(xrd.getSampleFiles()[1])
#' d = subset(d, theta > 30 & theta < 42)
#' p = xrd.peakEstimate(d$theta, d$I)
#' plot(d$theta, d$I, col='red'); abline(v=p[3], lwd=2)
#' abline(h=p[1], col='darkgrey')
#' p
#'
#' @importFrom stats approx
#' @importFrom ggplot2 ggplot geom_point aes
#'
#' @export
xrd.peakEstimate <- function(twoTheta, I, verbose=FALSE) {
  n1 = data.frame(th = twoTheta, I)

  # approximate the data set, so there is less
  # noise in the data
  q = approx(n1$th, n1$I, n=20)

  # find the position of the largest peak and its angle
  peakNo = which(q$y == max(q$y))
  th0 = q$x[peakNo]

  # find the background position and amplitude
  b0 = min(q$y)
  A0 = max(q$y) - b0

  # estimate the peak width
  ts0 = (q$y[peakNo + 2] - b0) / A0
  s0 = -(q$x[peakNo+2] - th0)/(sqrt(2)*log(ts0))

  if (verbose) {
    n2 = n1
    n1$type = 'exp'
    n2$type = 'fit'
    n2$I = b0 + A0*exp(-(n2$th-th0)^2/(2*s0*s0))
    n = rbind(n1,n2)
    ggplot(n, aes(th,I,col=type)) +
      geom_point() -> g
    print(g)
  }


  list(b0=b0, A0=A0, th0=th0, s0=s0)
}
