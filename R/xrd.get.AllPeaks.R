#' Finds all the peaks in a spectrum
#'
#' @param TwoTheta angle
#' @param Intensity intensity signal
#' @param min.Prominence minimum prominence in percent (optional)
#' @param Try.Sigma vector with peak widths used to start fitting (optional)
#' @param Range range to search for peaks (optional)
#' @return vector with peak positions
#' @examples
#'
#' \donttest{
#' filename = xrd.getSampleFiles()[1]
#' d = xrd.read.ASC(filename)
#' peak.list = xrd.get.AllPeaks(d$theta, d$I)
#' }
#'
#' @export
xrd.get.AllPeaks <- function(TwoTheta, Intensity,
                             min.Prominence = 5,
                             Try.Sigma = c(0.1,0.2,0.15),
                             Range = c(0,90)) {
  step.size = 0.4 # search peaks with this step size
  if (Range[1]==0) { theta.min = min(TwoTheta) + step.size/2 }
  if (Range[2]==90) { theta.max = max(TwoTheta) - step.size/2 }
  if ((theta.max-theta.min)<1) { return(c()) }
  peakPos.list = seq(from=theta.min, to=theta.max, by=step.size)

  p = c()
  for(ang in peakPos.list) {
    p1 = xrd.get.PeakStats(TwoTheta, Intensity, ang)
    if(length(p1)==8) {
      if (xrd.get.PeakProminence(p1) > min.Prominence) {
        p = c(p, p1[3])
      }
    }
  }
  p
}
