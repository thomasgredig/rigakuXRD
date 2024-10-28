#' Convert xrd data to a data frame
#' @param x xrd S3 class
#' @param ... dots
#' @export
as.data.frame.xrd <- function(x, ...) {
  if(!inherits(x,"xrd")) return(x)
  data.frame(theta = x$theta,
                I = x$I,
                I.meas = x$I.meas,
                loop = x$loop)
}

#' Filters XRD data
#' @param x xrd S3 class
#' @param th_min minimum theta angle
#' @param th_max maximum theta angle
#' @importFrom dplyr "%>%" filter
#' @importFrom rlang .data
#' @export
xrd_filter <- function(x, th_min, th_max) {
  isXRD <- inherits(x, "xrd")
  x <- as.data.frame(x)
  x <- x %>% filter(.data$theta >= th_min & .data$theta <= th_max)
  if(isXRD) class(x) <- 'xrd'
  x
}

#' Check whether we have a xrd S3 class object or vectors
#' @param TwoTheta either an xrd S3 object or a vector with two theta data
#' @param Intensity \code{NULL} if xrd S3 object, otherwise intensity vector
#' @noRd
check_dataXRD <- function(TwoTheta, Intensity=NULL) {
  if(inherits(TwoTheta,"xrd")) {
    d = data.frame(theta = TwoTheta$theta, I = TwoTheta$I)
  } else {
    if(inherits(TwoTheta, "data.frame")) {
      d = data.frame(theta = TwoTheta$theta, I = TwoTheta$I)
    } else {
      if (length(Intensity) != length(TwoTheta)) warning("TwoTheta and Intensity must have same length.")
      d = data.frame(theta = TwoTheta, I = Intensity)
    }
  }
  d
}
