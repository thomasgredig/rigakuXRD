#' Finds all the peaks in a spectrum
#'
#' @param TwoTheta angle
#' @param Intensity intensity signal
#' @param min.Prominence minimum prominence in percent (optional)
#' @param Try.Sigma vector with peak widths used to start fitting (optional)
#' @param Range range to search for peaks (optional)
#' @param verbose logical, if \code{TRUE} outputs additional info
#'
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
                             Try.Sigma = c(0.2,0.1,0.05,0.3),
                             deltaTheta = 5,
                             Range = c(0,90),
                             verbose = FALSE) {
  filename = xrd.getSampleFiles(fileExt='asc')
  d = xrd.read.ASC(filename)
  TwoTheta = d$theta
  Intensity = d$I
  d = data.frame(TwoTheta, Intensity)

  step.size = 2 # search peaks with this step size
  if (Range[1]==0) { theta.min = min(TwoTheta) + step.size/2 }
  if (Range[2]==90) { theta.max = max(TwoTheta) - step.size/2 }
  if ((theta.max-theta.min)<1) { return(c()) }
  peakPos.list = c(seq(from=theta.min, to=theta.max, by=step.size),
                   seq(from=theta.min+(step.size/2), to=theta.max-(step.size/2), by=step.size))

  pk = c()

  for(ang in peakPos.list) {
    print(paste("Checking: ",ang,"deg"))
    n1 = subset(d, TwoTheta >= (ang-deltaTheta) & TwoTheta<= (ang+deltaTheta))
    # plot(n1$TwoTheta, n1$Intensity)
    p1 = xrd.find.Peak(n1$TwoTheta, n1$Intensity,
                       Try.Sigma = Try.Sigma,
                       peakPos = ang,
                       verbose = verbose)
    if(!is.na(p1)) {
        pk = c(pk, p1)
    }
  }
  sort(pk)
}
