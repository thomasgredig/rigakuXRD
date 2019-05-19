#' Returns size estimate from Debye-Schrerrer Equation
#'
#' @param PeakStats vector with the peak statistics
#' @param Lambda wavelength of x-ray in (nm)
#' @return peak width in nanometer
#' @examples
#' filename = system.file("extdata", "2Theta.asc", package='rigakuXRD')
#' d = xrd.read.ASC(filename)
#' q = xrd.get.PeakStats(d$theta, d$I, 38.2)
#' xrd.get.PeakProminence(q)
#'
#' @export
xrd.get.DebyeScherrer <- function(PeakStats,
                                  Lambda = 0.154) {
  # https://en.wikipedia.org/wiki/Full_width_at_half_maximum
  # https://en.wikipedia.org/wiki/Scherrer_equation
  K = 0.9
  beta = 2.355*PeakStats[4]    # assuming Gaussian
  Theta = PeakStats[3]*pi/180  # in radians
  particle.size = K*Lambda/(beta*cos(Theta))
  particle.size
}
