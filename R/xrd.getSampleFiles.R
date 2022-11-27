#' Returns XRD sample files
#'
#' @description
#' returns sample XRD  from library
#'
#' @param fileExt file extension, could be \code{NA} for all, or "asc", "txt", "ras", "rasx"
#' @return vector with path/filename to sample XRD data files
#'
#' @author Thomas Gredig
#' @examples
#' file.list = xrd.getSampleFiles()
#' print(paste("Found",length(file.list),"sample files:"))
#' basename(file.list)
#'
#' @export
xrd.getSampleFiles <- function(fileExt = NA) {
  pfad = xrd.getSamplePath()
  file.list = dir(pfad)
  if (!is.na(fileExt)) file.list = file.list[grep(paste0(fileExt,"$"), file.list)]
  file.path(pfad, file.list)
}


#' Return sample path for XRD data files
#'
#' @description
#' returns sample XRD path from library
#'
#' @return path for sample XRD data files
#'
#' @author Thomas Gredig
#' @examples
#' pfad = xrd.getSamplePath()
#' @export
xrd.getSamplePath <- function() {
  system.file("extdata",package="rigakuXRD")
}
