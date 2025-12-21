#' XRD Filenames with Sample Data
#'
#' @description
#' Returns filenames with XRD sample data stored in different formats.
#' These data sets help the user quickly get started with example codes
#' for this package. It can also be used for testing and debugging purposes.
#'d
#'
#' @param fileExt XRD filename extension: "asc", "txt", "ras", "rasx"
#' @return vector with path and filename to sample XRD data files
#'
#' @author Thomas Gredig
#' @examples
#' file_list = xrd_sample_files()
#' basename(file_list)
#'
#' @export
xrd_sample_files <- function(fileExt = NULL) {
  pfad = system.file("extdata",package="rigakuXRD")
  file.list = dir(pfad)
  if (!is.null(fileExt)) file.list = file.list[grep(paste0(fileExt,"$"), file.list)]
  file.path(pfad, file.list)
}


#' @rdname xrd_sample_files
#' @export
xrd.getSampleFiles <- function(fileExt = NULL) {
  .Deprecated("xrd_sample_files", package = "rigakuXRD")
  xrd_sample_files(fileExt = fileExt)
}

