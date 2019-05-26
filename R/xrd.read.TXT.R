#' Reads the TXT Rigaku XRD file with a Header
#' @param filename filename including path
#' @return data frame with XRD data
#' @examples
#'
#' filename = system.file("extdata", "2Theta.asc", package='rigakuXRD')
#' d = xrd.read.TXT(filename)
#'
#' @import utils
#' @import stringr
#'
#' @export
xrd.read.TXT <- function(filename) {
  if(file.exists(filename)==FALSE) { warning(paste('File does not exist:',filename)) }
  data = read.csv(file=filename, stringsAsFactors=FALSE)
  as.vector(unlist(data))->p
  p[grep('^\\*',p)] -> d.header
  strsplit(gsub('^\\*','',d.header),'\t') -> p1
  secondValue <- function(x) { if(length(x)==2){x[[2]]}else{""} }
  # this is the header of the file
  q1 = data.frame(
    name = unlist(lapply(p1,'[[',1)),
    value = unlist(lapply(p1, secondValue))
  )
  # find column names
  p1 = p[grep('^#',p)][1]
  col.names = unlist(strsplit(gsub('^#','',p1),'='))
  # get data
  start.row = grep('^#',p)[1]+1
  p1 = strsplit(p[start.row:length(p)],'\t')
  d = data.frame(
    v1 = as.numeric(unlist(lapply(p1,'[[',1))),
    v2 = as.numeric(unlist(lapply(p1, secondValue)))
  )
  names(d) = col.names
  list('data' = d,
       'header' = q1)
}
