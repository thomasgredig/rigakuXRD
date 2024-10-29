#' Finds all peaks in an XRD spectrum
#'
#' @description
#' Attempts to find peaks in an XRD spectrum and return a list of angles with
#' the peak locations. (Use with caution, might find additional peaks.)
#'
#'
#' @param data data frame with intensity and 2 theta angle.
#' @param min.Prominence minimum prominence in percent (optional).
#' @param Try.Sigma vector with peak widths used to start fitting (optional).
#' @param deltaTheta search area around main peak (optional).
#' @param Range range to search for peaks (optional).
#'
#' @return sorted vector with peak positions with 0.01 degree precision.
#'
#' @seealso [xrd.find.Peak()]
#'
#' @examples
#'
#' filename = xrd.getSampleFiles('asc')
#' d = xrd.import(filename)
#' xrd.get.AllPeaks(d, deltaTheta = 2, Try.Sigma = c(0.1),  Range = c(42, 46))
#'
#' @export
xrd.get.AllPeaks <- function(data,
                             min.Prominence = 5,
                             Try.Sigma = c(0.2,0.1,0.05,0.3),
                             deltaTheta = 5,
                             Range = c(0,90)) {
  # verify that data has consistent format
  dataXRD <- check_dataXRD(data)
  # use a filter to limit range of data
  d <- xrd_filter(dataXRD, Range[1], Range[2])

  step.size = 2 # search peaks with this step size
  theta.min = min(d$TwoTheta) + step.size/2
  theta.max = max(d$TwoTheta) - step.size/2

  pk = c()
  if ((theta.max-theta.min)<1) {
    warning("XRD data has insufficient angular range.")
    return(pk)
  }

  peakPos.list = c(seq(from=theta.min, to=theta.max, by=step.size),
                   seq(from=theta.min+(step.size/2), to=theta.max-(step.size/2), by=step.size))

  for(ang in peakPos.list) {
    n1 <- xrd_filter(d, ang-deltaTheta, ang+deltaTheta)

    p1 = xrd.find.Peak(n1$TwoTheta, n1$I,
                       Try.Sigma = Try.Sigma,
                       peakPos = ang)
    if(!is.na(p1)) {
        pk = c(pk, p1)
    }
  }
  # sort the peak positions and only keep with a precision of 0.01 deg
  sort(unique(round(pk,2)))
}
