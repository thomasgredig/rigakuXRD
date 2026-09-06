#' S3 method to convert class 'xrd' to a data frame
#'
#' @param x xrd object
#' @param ... additional arguments passed to as.data.frame
#'
#' @return A data frame with the same number of rows as \code{x}. The
#'   columns are \code{TwoTheta} (2theta angle in degrees), \code{I}
#'   (normalized intensity), \code{I.meas} (measured intensity), and
#'   \code{loop} (measurement loop identifier), plus any columns supplied
#'   through \code{...}. The returned object has class \code{data.frame}.
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
#' @return A filtered object containing rows whose \code{TwoTheta} values
#'   fall between \code{th_min} and \code{th_max}, inclusive. If \code{x}
#'   inherits from \code{xrd}, the result is a class \code{xrd} data frame;
#'   otherwise it is a class \code{data.frame} object. The columns retain
#'   the XRD representation, including \code{TwoTheta}, \code{I},
#'   \code{I.meas}, and \code{loop}.
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
#' @return A list with class \code{xrd} and components \code{TwoTheta},
#'   \code{I}, \code{I.meas}, and \code{loop}. These contain, respectively,
#'   the 2theta angles in degrees, normalized intensity, measured intensity,
#'   and integer measurement-loop identifiers.
#'
#' @export
create_xrd <- function(TwoTheta, I, I.meas, loop) {
  if (!is.numeric(TwoTheta)) stop("TwoTheta must be numeric")
  if (!is.numeric(I)) stop("I must be numeric")
  if (!is.numeric(I.meas)) stop("I.meas must be numeric")
  if (!is.integer(loop)) stop("loop must be integer")

  obj <- list(TwoTheta = TwoTheta, I = I, I.meas = I.meas, loop = loop)
  class(obj) <- "xrd"

  return(obj)
}


#' Print an 'xrd' object as a data frame
#'
#' @param x xrd object.
#' @param ... further arguments passed to or from other methods.
#'
#' @return The data-frame representation of \code{x}, returned invisibly
#'   after printing. The display consists of an \code{"xrd object:"} label
#'   followed by columns \code{TwoTheta}, \code{I}, \code{I.meas}, and
#'   \code{loop}.
#'
#' @export
print.xrd <- function(x, ...) {
  message("xrd object:")
  df_x <- as.data.frame(x)
  print(df_x, row.names=FALSE)
}
