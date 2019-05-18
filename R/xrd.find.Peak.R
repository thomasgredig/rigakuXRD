#' Finds a peak
#'
#' @param TwoTheta angle
#' @param Intensity intensity signal
#' @param PeakPos approximate angle of peak position
#' @return fit peak position
#' @examples
#' filename = system.file("extdata", "2Theta.asc", package='rigakuXRD')
#' xrd.read.ASC(filename)
#' xrd.find.Peak(d$theta, d$I, 38.2)
#'
#' @export
xrd.find.Peak <- function(TwoTheta, Intensity, PeakPos) {
  d = data.frame(TwoTheta, I = Intensity)
  n1 = subset(d, TwoTheta > (PeakPos-1) & TwoTheta < (PeakPos+1))
  p1 = n1$TwoTheta[which.max(n1$I)]
  background = min(n1$I)
  A1 = max(n1$I) - background
  nls(data = n1,
      I ~ b + A*exp(-(TwoTheta-t0)^2/(2*sigma*sigma)),
      start = list(b=background, A = A1, t0=p1, sigma=0.2)) -> fit
  summary(fit)$coef[3]
}
