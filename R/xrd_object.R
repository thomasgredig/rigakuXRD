#' Convert xrd data to a data frame
#' @param x xrd S3 class
#' @param ... additional arguments passed to as.data.frame
#' @export
as.data.frame.xrd <- function(x,...) {
  data.frame(TwoTheta = x$TwoTheta,
                I = x$I,
                I.meas = x$I.meas,
                loop = x$loop, ...)
}

#' Filters XRD data
#' @param x xrd S3 class
#' @param th_min minimum theta angle
#' @param th_max maximum theta angle
#'
#' @importFrom dplyr "%>%" filter
#' @importFrom rlang .data
#' @importFrom cli cli_warn
#' @export
xrd_filter <- function(x, th_min, th_max) {
  if(th_max <= th_min) {
    temp <- th_max; th_max = th_min; th_min = temp
    cli_warn("max theta is smaller than min theta.")
  }
  isXRD <- inherits(x, "xrd")
  x <- as.data.frame(x)
  x <- x %>% filter(.data$TwoTheta >= th_min & .data$TwoTheta <= th_max)
  if(isXRD) class(x) <- 'xrd'
  x
}




#' Create an xrd S3 object
#'
#' @param TwoTheta vector of two theta angles from xrd spectrum
#' @param I intensity (counts per second)
#' @param I.meas measured intentisty
#' @param loop integer of a specific loop
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


#' Print xrd object as table
#'
#' @param x xrd object.
#' @param ... further arguments passed to or from other methods.
#' @export
print.xrd <- function(x, ...) {
  message("xrd object:")
  df_x <- as.data.frame(x)
  print(df_x, row.names=FALSE)
}

