#' Returns the Peak Prominence
#'
#' @param PeakStats vector with the peak statistics
#' @return percentage of the peak height with respect to background
#' @examples
#' filename = xrd.getSampleFiles()[1]
#' d = xrd.read.ASC(filename)
#' q = xrd.get.PeakStats(d$theta, d$I, 38.2)
#' xrd.get.PeakProminence(q)
#'
#' @export
xrd.get.PeakProminence <- function(PeakStats) {
  signif(PeakStats[2]/PeakStats[1]*100,3)
}
