#' Scherrer equation
#'
#' Returns the particle size or film thickness as calculated with
#' the Scherrer equation; units of Angstrom
#'
#' @param PeakStats vector with the peak statistics
#' @param Lambda wavelength of x-ray in (A)
#' @return peak width in Angstrom
#' @examples
#' filename = xrd.getSampleFiles()[1]
#' d = xrd.read.ASC(filename)
#' ds = xrd.get.PeakStats(d$theta, d$I, 38.2)
#' xrd.get.DebyeScherrer(ds)
#'
#' @export
xrd.get.DebyeScherrer <- function(PeakStats,
                                  Lambda = 1.5406) {
  # https://en.wikipedia.org/wiki/Full_width_at_half_maximum
  # https://en.wikipedia.org/wiki/Scherrer_equation
  K = 0.9
  beta = 2.355*PeakStats[4]*pi/180    # assuming Gaussian
  Theta = (PeakStats[3]*pi/180)/2     # in radians
  particle.size = K*Lambda/(beta*cos(Theta))
  particle.size
}
