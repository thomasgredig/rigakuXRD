#' Finds All Peaks in a Spectrum
#'
#' @description
#' Attempts to find peaks in an XRD spectrum and return a list of angles with
#' the peak locations. (This function is highly experimental and does not
#' work well.)
#'
#'
#' @param data data frame with intensity and 2 theta angle
#' @param min.Prominence minimum prominence in percent (optional)
#' @param Try.Sigma vector with peak widths used to start fitting (optional)
#' @param deltaTheta search area around main peak
#' @param Range range to search for peaks (optional)
#' @param verbose logical, if \code{TRUE} outputs additional info
#'
#' @return vector with peak positions
#'
#' @seealso [xrd.find.Peak()]
#' @importFrom dplyr "%>%" filter
#'
#' @examples
#'
#' filename = xrd.getSampleFiles('asc')
#' d = xrd.read.ASC(filename)
#' xrd.get.AllPeaks(d, deltaTheta = 2, Try.Sigma = c(0.1),  Range = c(42, 46))
#'
#' @export
xrd.get.AllPeaks <- function(data,
                             min.Prominence = 5,
                             Try.Sigma = c(0.2,0.1,0.05,0.3),
                             deltaTheta = 5,
                             Range = c(0,90),
                             verbose = FALSE) {
  # XRD data
  d <- data.frame(TwoTheta = data$theta,
                  Intensity = data$I) %>%
    filter(TwoTheta >= Range[1] & TwoTheta <= Range[2])

  step.size = 2 # search peaks with this step size
  theta.min = min(d$TwoTheta) + step.size/2
  theta.max = max(d$TwoTheta) - step.size/2


  peakPos.list = c(seq(from=theta.min, to=theta.max, by=step.size),
                   seq(from=theta.min+(step.size/2), to=theta.max-(step.size/2), by=step.size))

  pk = c()

  if ((theta.max-theta.min)<1) {
    warning("Data has insufficient angle range.")
    return(pk)
  }

  for(ang in peakPos.list) {
    n1 <- d %>% filter(TwoTheta >= (ang-deltaTheta) & TwoTheta<= (ang+deltaTheta))

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
