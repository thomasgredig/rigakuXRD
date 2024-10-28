#' Applies Scherrer Equation to PeakStats
#'
#' Returns the particle size or film thickness as calculated with
#' the Scherrer equation; units of Angstrom, for more information
#' see [Scherrer Equation](https://en.wikipedia.org/wiki/Scherrer_equation)
#'
#' @param PeakStats vector with the peak statistics
#' @param Lambda wavelength of x-ray in (A)
#' @param K geometrical factor, usually a number between 0.9 and 1.0
#'
#' @return peak width in Angstrom
#'
#' @seealso [xrd.get.PeakStats()]
#' @examples
#' filename = xrd.getSampleFiles('asc')
#' d = xrd.import(filename)
#' ds = xrd.get.PeakStats(d$TwoTheta, d$I, 38.2)
#' xrd.get.DebyeScherrer(ds)
#'
#' @export
xrd.get.DebyeScherrer <- function(PeakStats,
                                  Lambda = 1.5406,
                                  K = 0.9) {
  # https://en.wikipedia.org/wiki/Full_width_at_half_maximum
  # https://en.wikipedia.org/wiki/Scherrer_equation

  beta = 2.355*PeakStats[4]*pi/180    # assuming Gaussian
  Theta = (PeakStats[3]*pi/180)/2     # in radians
  particle.size = K*Lambda/(beta*cos(Theta))
  particle.size
}
