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




#' Print information
#' @importFrom cli cli_inform
#' @importFrom glue glue
#' @noRd
xrd_inform <- function(msg) {
  is_debug_mode <- (getOption("verbose", default = FALSE) == TRUE)
  if (is_debug_mode) {
    # Evaluate the message within the calling environment
    eval_message <- glue::glue(msg, .envir = parent.frame())
    cli_inform(eval_message)
  }
}
