#' Check whether we have a XRD S3 class object or vectors
#' @param TwoTheta vector of two theta angles from XRD spectrum OR XRD object.
#' @param Intensity \code{NULL} if XRD S3 object, otherwise XRD intensity vector
#' @noRd
check_dataXRD <- function(TwoTheta, Intensity=NULL) {
  if(inherits(TwoTheta,"xrd")) {
    d = data.frame(TwoTheta = TwoTheta$TwoTheta, I = TwoTheta$I)
  } else {
    if(inherits(TwoTheta, "data.frame")) {
      if("theta" %in% names(TwoTheta)) {
        d = data.frame(TwoTheta = TwoTheta$theta, I = TwoTheta$I)
      } else {
        d = data.frame(TwoTheta = TwoTheta$TwoTheta, I = TwoTheta$I)
      }
    } else {
      if (length(Intensity) != length(TwoTheta)) warning("TwoTheta and Intensity must have same length.")
      d = data.frame(TwoTheta = TwoTheta, I = Intensity)
    }
  }
  d
}




#' Verbose output for debugging
#' @importFrom cli cli_inform
#' @importFrom glue glue
#' @noRd
xrd_inform <- function(msg) {
  is_verbose_mode <- (getOption("verbose", default = FALSE) == TRUE)
  if (is_verbose_mode) {
    eval_message <- glue::glue(msg, .envir = parent.frame())
    cli_inform(eval_message)
  }
}
