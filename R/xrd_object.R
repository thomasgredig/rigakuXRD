#' S3 method to convert class 'xrd' to a data frame
#'
#' @param x xrd object
#' @param ... additional arguments passed to as.data.frame
#'
#' @returns as.data.frame returns a data frame for xrd object
#'   and includes columns with the diffraction angle \code{TwoTheta} and \code{Intensity}
#' @export
as.data.frame.xrd <- function(x,...) {
  data.frame(TwoTheta = x$TwoTheta,
                I = x$I,
                I.meas = x$I.meas,
                loop = x$loop, ...)
}

#' Filters XRD data by diffraction angle
#'
#' @param x 'xrd' S3 class
#' @param th_min minimum theta angle
#' @param th_max maximum theta angle
#'
#' @returns 'xrd' object with angle-truncated data set
#'
#' @importFrom dplyr "%>%" filter
#' @importFrom rlang .data
#' @importFrom cli cli_warn
#' @export
xrd_filter <- function(x, th_min, th_max) {
  if(th_max <= th_min) {
    temp <- th_max; th_max = th_min; th_min = temp
    cli_warn("Maximum theta is smaller than minimum theta.")
  }
  isXRD <- inherits(x, "xrd")
  x <- as.data.frame(x)
  x <- x %>% filter(.data$TwoTheta >= th_min & .data$TwoTheta <= th_max)
  if(isXRD) class(x) <- 'xrd'
  x
}




#' Create an 'xrd' S3 class
#'
#' @param TwoTheta vector of two theta angles from xrd spectrum
#' @param I intensity (counts per second)
#' @param I.meas measured intensity
#' @param loop integer of a specific loop
#'
#' @returns 'xrd' object with diffraction data containing angle and intensity
#'
#' @export
create_xrd <- function(TwoTheta, I, I.meas, loop) {
  # Ensure the parameters are of the correct type
  if (!is.numeric(TwoTheta)) stop("TwoTheta must be numeric")
  if (!is.numeric(I)) stop("I must be numeric")
  if (!is.numeric(I.meas)) stop("I.meas must be numeric")
  if (!is.integer(loop)) stop("loop must be integer")

  # Create the object
  obj <- list(TwoTheta = TwoTheta, I = I, I.meas = I.meas, loop = loop)

  # Assign the class name
  class(obj) <- "xrd"

  return(obj)
}


#' Print an 'xrd' object as a data frame
#'
#' @param x xrd object.
#' @param ... further arguments passed to or from other methods.
#'
#' @returns outputs an 'xrd' object as a data frame
#'
#' @export
print.xrd <- function(x, ...) {
  message("xrd object:")
  df_x <- as.data.frame(x)
  print(df_x, row.names=FALSE)
}

