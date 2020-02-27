#' Reads the RAS Rigaku XRD data
#' @param filename filename including path
#' @return data frame with XRD data
#' @examples
#'
#' filename = system.file("extdata", "2Theta.ras", package='rigakuXRD')
#' d = xrd.read.RAS(filename)
#'
#' @import utils
#' @import dplyr
#' @import tidyr
#'
#' @export
xrd.read.RAS <- function(filename) {
  if(file.exists(filename)==FALSE) { warning(paste('File does not exist:',filename)) }
  data = read.csv(file=filename, stringsAsFactors=FALSE, row.names=NULL,encoding = "UTF-8")
  p = as.vector(unlist(data))
  if(p[1]!="*RAS_HEADER_START") { warning(paste("File format is not RAS:",filename))}
  p.start = grep('*RAS_INT_START',p)
  p.end = grep('*RAS_INT_END',p)
  d1 = data.frame(
    n = p[(p.start+1):(p.end-1)],
    stringsAsFactors = FALSE
  )

  lines.comment = grep('^\\*',p)
  d.header = data.frame(n = gsub('^\\*','',p[lines.comment]), stringsAsFactors = FALSE) %>%
    separate(n, c('name','value'), " ")
  label.x = xrd.rasHeader.value(d.header,'DISP_TAB_NAME')
  label.y = xrd.rasHeader.value(d.header,'DISP_TITLE_Y')
  label.units = xrd.rasHeader.value(d.header,'DISP_UNIT_Y')

  d1 %>%
    separate(n, c(label.x,label.y,label.units), " ") %>%
    sapply(as.numeric)
}



xrd.read.rasHeader <- function(filename) {
  if(file.exists(filename)==FALSE) { warning(paste('File does not exist:',filename)) }
  data = read.csv(file=filename, stringsAsFactors=FALSE, row.names=NULL,encoding = "UTF-8")
  p = as.vector(unlist(data))
  if(p[1]!="*RAS_HEADER_START") { warning(paste("File format is not RAS:",filename))}
  lines.comment = grep('^\\*',p)
  d1 = data.frame(n = gsub('^\\*','',p[lines.comment]), stringsAsFactors = FALSE)

  d1 %>%
    separate(n, c('name','value'), " ")
}

xrd.rasHeader.value <- function(d,item='FILE_MEMO') {
  d$value[grep(item,d$name)]
}
