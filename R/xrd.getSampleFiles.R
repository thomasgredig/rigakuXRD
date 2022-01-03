#' Get sample XRD files
#'
#' @description
#' returns sample XRD  from library
#' @return vector with path/filename to sample XRD data files
#' @author Thomas Gredig
#' @examples
#' file.list = xrd.getSampleFiles()
#' print(paste("Found",length(file.list),"sample files."))
#' basename(file.list)
#' @export
xrd.getSampleFiles <- function() {
  pfad = xrd.getSamplePath()
  file.list = dir(pfad)
  file.path(pfad, file.list)
}


#' Return sample path for XRD data files
#'
#' @description
#' returns sample XRD path from library
#' @return path for sample XRD data files
#' @author Thomas Gredig
#' @examples
#' pfad = xrd.getSamplePath()
#' @export
xrd.getSamplePath <- function() {
  system.file("extdata",package="rigakuXRD")
}
