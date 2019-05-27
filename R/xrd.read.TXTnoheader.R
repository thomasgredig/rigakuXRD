#' Reads the TXT Rigaku XRD file with no header
#' @param filename filename including path
#' @return data frame with XRD data
#' @examples
#'
#' filename = system.file("extdata", "MnPcTheta.txt", package='rigakuXRD')
#' d = xrd.read.TXTnoheader(filename)
#'
#' @import utils
#'
#' @export
xrd.read.TXTnoheader <- function(filename) {
  if(file.exists(filename)==FALSE) { warning(paste('File does not exist:',filename)) }
  d = read.csv(file=filename, sep='\t', stringsAsFactors=FALSE, header=FALSE)
  q1 = c()
  names(d) = c('theta','I')

  list('data' = d,
       'header' = q1)
}
