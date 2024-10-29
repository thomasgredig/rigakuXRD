#' Export XRD Data into GenX Format
#'
#' @description
#' GenX uses the differential evolution algorithm for fitting X-ray
#' reflectivity data. This function normalizes the data and formats
#' x-ray reflectivity data, so that it can be imported with the GenX
#' code. The data is saved to a text file. More details are found at:
#' https://doi.org/10.1107/S1600576722006653
#'
#' @param dataXRD xrd Object
#' @param exportPath default is a temporary direction, stores the output file in this directory
#' @param minTheta minimum 2Theta to be included
#' @param maxTheta maximum 2Theta to be included in export
#' @param GenX_filename filename for output file
#' @returns file name of the exported GenX text file
#'
#' @importFrom utils write.table
#'
#' @seealso [xrd.import()]
#' @examples
#' filename = xrd.getSampleFiles(fileExt='asc')
#' d = xrd.import(filename, dataXRD=TRUE)
#' peak.pos = xrd.find.Peak(d, peakPos = 38.2)
#' plot(d)
#' abline(v=peak.pos,col='blue')
#'
#' @export
export2genx <- function(dataXRD, exportPath = NULL,
                        minTheta=0, maxTheta=4,
                        GenX_filename = 'xrd_data_genx_format.txt') {

  dataXRD <- check_dataXRD(dataXRD)
  if(is.null(exportPath)) exportPath = tempdir()
  # create filename for export
  fileExport <- file.path(exportPath, GenX_filename)

  dataXRD <- as.data.frame(dataXRD)
  max_Intensity <- max(dataXRD$I)

  df <- data.frame(
    TwoTheta = dataXRD$TwoTheta,
    I = dataXRD$I
  )
  df <- xrd_filter(df, minTheta, maxTheta)
  df$I = df$I / max_Intensity
  write.table(df, file=fileExport,
                sep = '\t',
              row.names = FALSE,
              col.names = FALSE)

  # return full filename
  fileExport
}

