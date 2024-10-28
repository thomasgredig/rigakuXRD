#' Convert xrd data to a data frame
#' @param x xrd S3 class
#' @param ... dots
#' @export
as.data.frame.xrd <- function(x, ...) {
  if(!inherits(x,"xrd")) return(x)
  data.frame(TwoTheta = x$TwoTheta,
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
  if(th_max <= th_min) {
    temp <- th_max; th_max = th_min; th_min = temp
    warning("max theta is smaller than min theta.")
  }
  isXRD <- inherits(x, "xrd")
  x <- as.data.frame(x)
  x <- x %>% filter(.data$TwoTheta >= th_min & .data$TwoTheta <= th_max)
  if(isXRD) class(x) <- 'xrd'
  x
}

#' Check whether we have a xrd S3 class object or vectors
#' @param TwoTheta either an xrd S3 object or a vector with two theta data
#' @param Intensity \code{NULL} if xrd S3 object, otherwise intensity vector
#' @noRd
check_dataXRD <- function(TwoTheta, Intensity=NULL) {
  if(inherits(TwoTheta,"xrd")) {
    d = data.frame(TwoTheta = TwoTheta$TwoTheta, I = TwoTheta$I)
  } else {
    if(inherits(TwoTheta, "data.frame")) {
      d = data.frame(TwoTheta = TwoTheta$TwoTheta, I = TwoTheta$I)
    } else {
      if (length(Intensity) != length(TwoTheta)) warning("TwoTheta and Intensity must have same length.")
      d = data.frame(TwoTheta = TwoTheta, I = Intensity)
    }
  }
  d
}



#' Create an xrd S3 object
#'
#' @param TwoTheta angle
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

#' Print xrd object
#'
#' @param x xrd object
#' @param ... dots
#' @export
print.xrd <- function(x, ...) {
  cat("xrd object:\n")
  df_x <- as.data.frame(x)
  print(df_x, row.names=FALSE)
}
