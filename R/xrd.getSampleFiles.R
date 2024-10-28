#' Get XRD File Names with Sample Data
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
#' basename(file.list)
#'
#' @export
xrd.getSampleFiles <- function(fileExt = NA) {
  pfad = system.file("extdata",package="rigakuXRD")
  file.list = dir(pfad)
  if (!is.na(fileExt)) file.list = file.list[grep(paste0(fileExt,"$"), file.list)]
  file.path(pfad, file.list)
}

