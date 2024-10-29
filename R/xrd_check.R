#' Check whether we have a xrd S3 class object or vectors
#' @param TwoTheta vector of two theta angles from xrd spectrum OR xrd object.
#' @param Intensity \code{NULL} if xrd S3 object, otherwise xrd intensity vector
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




#' Verbose output for debugging
#' @importFrom cli cli_inform
#' @importFrom glue glue
#' @noRd
xrd_inform <- function(msg) {
  is_verbose_mode <- (getOption("verbose", default = FALSE) == TRUE)
  if (is_verbose_mode) {
    # Evaluate the message within the calling environment
    eval_message <- glue::glue(msg, .envir = parent.frame())
    cli_inform(eval_message)
  }
}
