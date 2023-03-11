#' Fits the peak to a Gaussian
#'
#' usually run after a peak is
#' identified through its location
#'
#' @param TwoTheta angle
#' @param Intensity intensity signal
#' @param PeakPos approximate angle of peak position
#' @param Try.Sigma vector with peak widths used to start fitting
#' @param verbose logical, if \code{TRUE} output additional information
#'
#' @return background, amplitude, position, width + 4 std. errors
#' @examples
#' filename = xrd.getSampleFiles(fileExt='asc')
#' d = xrd.read.ASC(filename)
#' xrd.get.PeakStats(d$theta, d$I, 38.2)
#'
#' @importFrom stats nls sd
#' @export
xrd.get.PeakStats <- function(TwoTheta, Intensity, PeakPos,
                              Try.Sigma = c(0.1,0.4,0.2,0.15),
                              verbose = FALSE) {
  d = data.frame(TwoTheta, I = Intensity)
  for(peak.width in Try.Sigma) {
    fit <- NULL
    n1 = subset(d, TwoTheta > (PeakPos-2.5*peak.width) &
                  TwoTheta < (PeakPos+2.5*peak.width))
    if (verbose) print(paste("Set from 2q =",
                             min(n1$TwoTheta), "to",
                             max(n1$TwoTheta)))
    if(nrow(n1)>5) {
      p1 = n1$TwoTheta[which.max(n1$I)]
      background = min(n1$I)
      A1 = max(n1$I) - background
      if (A1 > (3*sd(n1$I))) {
        if (verbose) {
          print(paste("background=",background,
                      " A=",A1,
                      " t0=",p1,
                      " sigma=0.2"))
        }
        try(fit <-
              nls(data = n1,
                  I ~ b + A*exp(-(TwoTheta-t0)^2/(2*sigma*sigma)),
                  start = list(b=background, A = A1, t0=p1, sigma=0.2))
        ); # does not stop in case of error
      } else {
        fit = NULL
      }
      if (!is.null(fit)) break;
    }
  }
  if(is.null(fit)) { return(NA) }
  summary(fit)$coeff[1:8]
}
