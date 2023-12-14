#' Fringe Thickness
#'
#' @returns thickness in units of Angstrom
#'
#' @examples
#' xrd.FringeThickness(66.22698, 66.32140)
#'
#' @export
xrd.FringeThickness <- function(theta1, theta2, lambda = 1.5406) {
  # see page 28: https://web.stanford.edu/group/glam/xlab/MatSci162_172/LectureNotes/08_High-Resolution%20XRD.pdf
  n1 = 2
  n2 = 1

  th1.rad = theta1/180*pi
  th2.rad = theta2/180*pi

  t = (n2 - n1)*lambda / (2*(sin(th1.rad/2) - sin(th2.rad/2)))

  t
}
