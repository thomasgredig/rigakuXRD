#' Returns size estimate from Debye-Schrerrer Equation
#'
#' @param PeakStats vector with the peak statistics
#' @param Lambda wavelength of x-ray in (A)
#' @return peak width in Angstrom
#' @examples
#' filename = system.file("extdata", "2Theta.asc", package='rigakuXRD')
#' d = xrd.read.ASC(filename)
#' xrd.get.DebyeScherrer(d$theta, d$I, 38.2)
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
