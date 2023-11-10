#' Export XRD data to GenX format
#'
#' @param data XRD data, usually filter(dataXRD, ID==7)
#' @param exportPath default is current directory, but can be defined to store output file in a different directory
#' @param minTheta minimum 2q to be included
#' @param maxTheta maximum 2q to be included in export
#' @return file name for export
#'
#' @importFrom dplyr '%>%' select mutate filter
#' @importFrom utils write.table
#' @export
export2genx <- function(data, exportPath = '.', minTheta=0, maxTheta=4) {
  # create filename for export
  fileExport <- file.path(exportPath, 'xrd_data_genx_format.txt')
  theta <- NULL
  #export
  data %>%
    select(theta, I) %>%
    filter(theta > minTheta) %>%
    filter(theta < maxTheta) %>%
    mutate(I = I/max(I)) %>%
    write.table(file=fileExport,
                sep = '\t',
              row.names = FALSE,
              col.names = FALSE)

  # return filename
  fileExport
}
