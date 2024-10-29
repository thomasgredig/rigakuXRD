#' Estimate highest peak in selected spectrum
#'
#' @description
#' Simple peak position estimate, yields largest peak
#' in a data set. This is not a fit, just an
#' estimate based on the data at hand.
#' It returns the background (b0), the amplitude (A0), the
#' peak position (th0), and the half-width sigma (s0).
#'
#' @param TwoTheta vector of two theta angles from xrd spectrum
#' @param Intensity \code{NULL} if xrd S3 object, otherwise xrd intensity vector
#'
#' @returns
#' 4 estimated parameters: background (b0), amplitude (A0), peak position (th0), and half-width (s0)
#'
#' @examples
#' d = xrd.import(xrd.getSampleFiles('asc'), TRUE)
#' d = xrd_filter(d, 30,42)
#' p = xrd.peakEstimate(d)
#' plot(d); abline(v=p[3], lwd=2)
#' abline(h=p[1], col='darkgrey')
#' p
#'
#' @importFrom stats approx
#' @importFrom ggplot2 ggplot geom_point aes
#'
#' @export
xrd.peakEstimate <- function(TwoTheta, Intensity = NULL) {
  dXRD <- check_dataXRD(TwoTheta, Intensity)

  TwoTheta <- dXRD$TwoTheta
  I <- dXRD$I

  # approximate the data set, so there is less
  # noise in the data
  q <- approx(TwoTheta, I, n=20)

  # find the position of the largest peak and its angle
  peakNo <- which(q$y == max(q$y))
  th0  <- q$x[peakNo]

  # find the background position and amplitude
  b0 <- min(q$y)
  A0 <- max(q$y) - b0

  # estimate the peak width
  ts0 <- +(q$y[peakNo + 2] - b0) / A0
  s0  <- -(q$x[peakNo + 2] - th0) / (sqrt(2)*log(ts0))

  # if (verbose) {
  #   n1 <- data.frame(TwoTheta, I, tp='exp')
  #   n2 <- data.frame(TwoTheta, I, tp='fit')
  #   n2$I  <-  b0 + A0*exp(-(n2$TwoTheta-th0)^2/(2*s0*s0))
  #
  #   n <- rbind(n1,n2)
  #
  #   # ggplot(n, aes(TwoTheta, I,  col=.data$tp)) +
  #   #   geom_point() -> g
  #   # print(g)
  # }


  list(b0=b0, A0=A0, th0=th0, s0=s0)
}
