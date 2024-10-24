#' Export XRD Data into GenX Format
#'
#' @description
#' GenX uses the differential evolution algorithm for fitting X-ray
#' reflectivity data. This function normalizes the data and formats
#' x-ray reflectivity data, so that it can be imported with the GenX
#' code. The data is saved to a text file. More details are found at:
#' https://doi.org/10.1107/S1600576722006653
#'
#' @param data XRD data frame, see rawData package
#' @param exportPath default is a temporary direction, stores the output file in this directory
#' @param minTheta minimum 2q to be included
#' @param maxTheta maximum 2q to be included in export
#' @param GenX_filename filename for output file
#' @returns file name for export
#'
#' @importFrom dplyr '%>%' select mutate filter
#' @importFrom utils write.table
#' @examples
#' filename <- xrd.getSampleFiles('txt')
#' data <- xrd.import(filename)
#' genx_file = export2genx(data)
#'
#' @export
export2genx <- function(data, exportPath = NULL, minTheta=0, maxTheta=4, GenX_filename = 'xrd_data_genx_format.txt') {
  if(is.null(exportPath)) exportPath = tempdir()
  # create filename for export
  fileExport <- file.path(exportPath, GenX_filename)
  theta <- NULL
  max_Intensity <- max(data$I)
  #export
  data %>%
    select(theta, I) %>%
    filter(theta > minTheta) %>%
    filter(theta < maxTheta) %>%
    mutate(I = I/max_Intensity) %>%
    write.table(file=fileExport,
                sep = '\t',
              row.names = FALSE,
              col.names = FALSE)

  # return full filename
  fileExport
}
