#' Export XRD data in GenX format
#'
#' @description
#' Exports X-ray diffraction (XRD) or reflectivity data to a plain-text
#' format compatible with the GenX fitting program, which uses a
#' differential evolution algorithm for model optimization. The data
#' are normalized and filtered to a specified 2theta range before export.
#'
#' @param dataXRD An \code{xrd} object or compatible data frame
#'   containing XRD data.
#' @param exportPath Character string giving the directory where the
#'   output file will be written. If \code{NULL}, a temporary directory
#'   is used.
#' @param minTheta Numeric scalar giving the minimum 2theta value to include
#'   in the export.
#' @param maxTheta Numeric scalar giving the maximum 2theta value to include
#'   in the export.
#' @param GenX_filename Character string specifying the name of the
#'   output text file.
#'
#' @details
#' The intensity data are normalized by the maximum intensity value
#' prior to export. The resulting file contains two tab-separated
#' columns (2theta and normalized intensity) with no header, as expected by
#' GenX.
#'
#' For details on the GenX method and file format, see
#' \doi{10.1107/S1600576722006653}.
#'
#' @return
#' Character string giving the full path to the exported GenX text file.
#'
#' @seealso
#' \code{\link{xrd_import}} for importing XRD data files.
#'
#' @examples
#' d <- xrd_import(xrd_sample_files("asc"))
#' output <- export2genx(d, minTheta = 35, maxTheta = 40)
#' file.exists(output)
#'
#' @importFrom utils write.table
#' @export
export2genx <- function(dataXRD, exportPath = NULL,
                        minTheta = 0, maxTheta = 4,
                        GenX_filename = "xrd_data_genx_format.txt") {

  dataXRD <- check_dataXRD(dataXRD)
  if (is.null(exportPath)) exportPath <- tempdir()

  # create filename for export
  fileExport <- file.path(exportPath, GenX_filename)

  dataXRD <- as.data.frame(dataXRD)
  max_Intensity <- max(dataXRD$I)

  df <- data.frame(
    TwoTheta = dataXRD$TwoTheta,
    I = dataXRD$I
  )

  df <- xrd_filter(df, minTheta, maxTheta)
  df$I <- df$I / max_Intensity

  write.table(
    df,
    file = fileExport,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE
  )

  # return full filename
  fileExport
}
