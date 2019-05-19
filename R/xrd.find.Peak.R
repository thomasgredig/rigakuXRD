#' Finds a peak
#'
#' @param TwoTheta angle
#' @param Intensity intensity signal
#' @param PeakPos approximate angle of peak position
#' @return fit peak position
#' @examples
#' filename = system.file("extdata", "2Theta.asc", package='rigakuXRD')
#' d = xrd.read.ASC(filename)
#' xrd.find.Peak(d$theta, d$I, 38.2)
#'
#' @import stats
#' @export
xrd.find.Peak <- function(TwoTheta, Intensity, PeakPos) {
  d = data.frame(TwoTheta, I = Intensity)
  n1 = subset(d, TwoTheta > (PeakPos-1) & TwoTheta < (PeakPos+1))
  if(nrow(n1)<5) { return(NA) }
  p1 = n1$TwoTheta[which.max(n1$I)]
  background = min(n1$I)
  A1 = max(n1$I) - background
  for(peak.width in (1:5*0.1)) {
    fit <- NULL
    try(fit <-
      nls(data = n1,
          I ~ b + A*exp(-(TwoTheta-t0)^2/(2*sigma*sigma)),
          start = list(b=background, A = A1, t0=p1, sigma=0.2))
    ); # does not stop in case of error
    if (!is.null(fit)) break;
  }
  if(is.null(fit)) { return(NA) }
  summary(fit)$coef[3]
}
