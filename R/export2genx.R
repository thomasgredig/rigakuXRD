#' Export XRD Data into GenX format
#'
#' @description
#' GenX uses the differential evolution algorithm for fitting X-ray
#' reflectivity data. The data is normalized and saved to a text (.txt)
#' file.
#'
#' @param data XRD data frame, see rawData package
#' @param exportPath default is current directory, but can be defined to store output file in a different directory
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
#' genx_file = export2genx(data, tempdir())
#' lines <- readLines(genx_file)
#' print(lines[1:4])
#'
#'
#' @export
export2genx <- function(data, exportPath = '.', minTheta=0, maxTheta=4, GenX_filename = 'xrd_data_genx_format.txt') {
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
