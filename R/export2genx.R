#' Export XRD Data into GenX Format
#'
#' @description
#' GenX uses the differential evolution algorithm for fitting X-ray
#' reflectivity data. This function normalizes the data and formats
#' x-ray reflectivity data, so that it can be imported with the GenX
#' code. The data is saved to a text file. More details are found at:
#' https://doi.org/10.1107/S1600576722006653
#'
#' @param dataXRD XRD data frame, see rawData package
#' @param exportPath default is a temporary direction, stores the output file in this directory
#' @param minTheta minimum 2Theta to be included
#' @param maxTheta maximum 2Theta to be included in export
#' @param GenX_filename filename for output file
#' @returns file name for export
#'
#' @importFrom dplyr "%>%" select mutate filter
#' @importFrom utils write.table
#' @importFrom rlang .data
#' @examples
#' filename <- xrd.getSampleFiles('txt')
#' data <- xrd.import(filename)
#' genx_file = export2genx(data)
#'
#' @export
export2genx <- function(dataXRD, exportPath = NULL,
                        minTheta=0, maxTheta=4,
                        GenX_filename = 'xrd_data_genx_format.txt') {

  # dataXRD <- check_dataXRD(dataXRD)
  if(is.null(exportPath)) exportPath = tempdir()
  # create filename for export
  fileExport <- file.path(exportPath, GenX_filename)

  dataXRD <- as.data.frame(dataXRD)
  max_Intensity <- max(dataXRD$I)
  #export
  dataXRD %>%
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

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
