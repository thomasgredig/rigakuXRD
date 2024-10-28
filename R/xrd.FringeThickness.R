#' Fringe Thickness Calculation
#'
#' @description
#' Given two angles (2Theta) and the x-ray wavelength, the
#' corresponding thickness in units of Angstrom is returned
#'
#'
#' @param theta1 first Two Theta (2Theta) angle in degree
#' @param theta2 second Two Theta (2Theta) angle in degree
#' @param lambda wave length of x-ray radiation in units of Angstrom
#'
#' @returns thickness in units of Angstrom
#'
#' @examples
#' # Should be 111.64 nm
#' xrd.FringeThickness(66.22698, 66.32140)
#'
#' @export
xrd.FringeThickness <- function(theta1, theta2, lambda = 1.5406) {
  n1 = 2
  n2 = 1

  # convert angles from degrees to radians
  omega1.rad <- (theta1/2)/180*pi
  omega2.rad <- (theta2/2)/180*pi

  (n2 - n1)*lambda / (2*(sin(omega1.rad) - sin(omega2.rad)))
}
