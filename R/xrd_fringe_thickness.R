#' Calculate film thickness from XRD/XRR fringe spacing
#'
#' @description
#' Computes the film thickness from two adjacent interference fringe
#' positions (2θ angles) and the X-ray wavelength.
#'
#' @param theta1 Numeric scalar giving the first 2θ angle (in degrees).
#' @param theta2 Numeric scalar giving the second 2θ angle (in degrees).
#' @param lambda Numeric scalar giving the X-ray wavelength (in Å).
#'   Defaults to Cu Kα radiation (\eqn{1.5406 \AA}).
#'
#' @details
#' The calculation assumes consecutive interference fringes and uses
#' the difference in the sine of the corresponding incident angles.
#'
#' @return
#' Numeric scalar giving the estimated film thickness in Å.
#'
#' @examples
#' # Expected result: approximately 1116 Å (111.6 nm)
#' xrd_fringe_thickness(66.22698, 66.32140)
#'
#' @export
xrd_fringe_thickness <- function(theta1, theta2, lambda = 1.5406) {
  n1 <- 2
  n2 <- 1

  # Convert angles from degrees (2θ) to radians (θ)
  omega1 <- (theta1 / 2) * pi / 180
  omega2 <- (theta2 / 2) * pi / 180

  (n2 - n1) * lambda / (2 * (sin(omega1) - sin(omega2)))
}


#' @rdname xrd_fringe_thickness
#' @export
xrd.FringeThickness <- function(theta1, theta2, lambda = 1.5406) {
  .Deprecated("xrd_fringe_thickness", package = "rigakuXRD")
  xrd_fringe_thickness(theta1 = theta1, theta2 = theta2, lambda = lambda)
}
