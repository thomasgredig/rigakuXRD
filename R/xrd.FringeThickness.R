#' Fringe Thickness Calculation
#'
#' @description
#' Given two angles (2 theta) and the x-ray wavelength, it computes the
#' corresponding thickness in units of Angstrom
#'
#'
#' @param theta1 first Two Theta angle in degree
#' @param theta2 second Two Theta angle in degree
#' @param lambda wave length of x-ray radiation in units of Angstrom
#'
#' @returns thickness in units of Angstrom
#'
#' @examples
#' print("Should be 111.64 nm")
#' xrd.FringeThickness(66.22698, 66.32140)
#'
#' @export
xrd.FringeThickness <- function(theta1, theta2, lambda = 1.5406) {
  # see page 28: https://web.stanford.edu/group/glam/xlab/MatSci162_172/LectureNotes/08_High-Resolution%20XRD.pdf
  n1 = 2
  n2 = 1

  omega1.rad = (theta1/2)/180*pi
  omega2.rad = (theta2/2)/180*pi

  t = (n2 - n1)*lambda / (2*(sin(omega1.rad) - sin(omega2.rad)))

  t
}
