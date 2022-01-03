#' Finds Peak Position from Gaussian
#'
#' Fits a Gaussian form to a peak
#'
#' @param TwoTheta angle
#' @param Intensity intensity signal
#' @param PeakPos approximate angle of peak position
#' @param Try.Sigma vector with peak widths used to start fitting
#' @return fit peak position
#' @examples
#' library(stringr)
#' filename = system.file("extdata", "2Theta.asc", package='rigakuXRD')
#' d = xrd.read.ASC(filename)
#' plot(d$theta,d$I,log='y',col='red')
#' peak.pos = xrd.find.Peak(d$theta, d$I, 38.2)
#' abline(v=peak.pos,col='blue')
#'
#' @import stats
#' @import stringr
#' @export
xrd.find.Peak <- function(TwoTheta, Intensity, PeakPos,
                          Try.Sigma = c(0.1,0.2,0.15)) {
  d = data.frame(TwoTheta, I = Intensity)
  for(peak.width in Try.Sigma) {
    fit <- NULL
    n1 = subset(d, TwoTheta > (PeakPos-2.5*peak.width) &
                  TwoTheta < (PeakPos+2.5*peak.width))
    if(nrow(n1)>5) {
      p1 = n1$TwoTheta[which.max(n1$I)]
      background = min(n1$I)
      A1 = max(n1$I) - background
      try(fit <-
            nls(data = n1,
                I ~ b + A*exp(-(TwoTheta-t0)^2/(2*sigma*sigma)),
                start = list(b=background, A = A1, t0=p1, sigma=0.2))
      ); # does not stop in case of error
      if (!is.null(fit)) break;
    }
  }
  if(is.null(fit)) { return(NA) }
  summary(fit)$coef[3]
}
