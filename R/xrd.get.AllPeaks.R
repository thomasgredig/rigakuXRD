empty.list = c()
#' Finds all the peaks in a spectrum
#'
#' @param TwoTheta angle
#' @param Intensity intensity signal
#' @param Try.Sigma vector with peak widths used to start fitting
#' @return vector with peak positions
#' @examples
#'
#' filename = system.file("extdata", "2Theta.asc", package='rigakuXRD')
#' d = xrd.read.ASC(filename)
#' peak.list = xrd.get.AllPeaks(d$theta, d$I)
#'
#' @export
xrd.get.AllPeaks <- function(TwoTheta, Intensity,
                             Try.Sigma = c(0.1,0.2,0.15)) {
  theta.min = min(TwoTheta)
  theta.max = max(TwoTheta)
  if ((theta.max-theta.min)<1) { return(empty.list) }
  peakPos.list = seq(from=theta.min, to=theta.max, by=0.8)

  p = c()
  for(ang in peakPos.list) {
    p1 = xrd.find.Peak(TwoTheta, Intensity, ang)
    if(!is.na(p1)) {
      p = c(p, p1)
    }
  }
  p
}
