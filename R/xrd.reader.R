#' Reads the Text converted XRD file with a Header
#'
#' @param filename filename including path
#' @return list with data and header
#' @examples
#' d = xrd.read.TxtWithHeader(filename)
#'
#' @export
xrd.read.TxtWithHeader <- function(filename) {
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




#' Reads the ASC Rigaku XRD file with a Header
#' @param filename filename including path
#' @return data frame with XRD data
#' @examples
#'
#' filename = system.file("extdata", "2Theta.asc", package='rigakuXRD')
#' d = xrd.read.ASC(filename)
#'
#' @import utils
#' @import stringr
#'
#' @export
xrd.read.ASC <- function(filename) {
  if(file.exists(filename)==FALSE) { warning(paste('File does not exist:',filename)) }
  data = read.csv(file=filename, stringsAsFactors=FALSE)
  as.vector(unlist(data))->p

  str_extract_all(p, '\\*{1}COUNT\t') -> a
  which(a=="*COUNT\t")->q
  as.numeric(gsub("\\D", "", p[q]))  -> b

  x.start = q+1
  x.end = q+b

  str_extract_all(p, '\\*{1}START\t') -> angle
  which(angle=="*START\t")->q

  seq=1:(length(q))*2
  as.numeric(unlist(strsplit(p[q],'='))[seq]) -> theta.start
  q=q+1
  as.numeric(unlist(strsplit(p[q],'='))[seq]) -> theta.stop
  q=q+1
  as.numeric(unlist(strsplit(p[q],'='))[seq]) -> theta.step
  q=q+2
  as.numeric(unlist(strsplit(p[q],'='))[seq]) -> theta.speed
  norm = theta.speed

  data=data.frame()
  for(num in 1:length(q)) {
    theta = seq(from=theta.start[num], to=theta.stop[num], by=theta.step[num])
    I = as.numeric(p[x.start[num]:x.end[num]])
    data = rbind(data, cbind(theta=theta, I = I/norm[num], I.meas = I, loop=num))

  }

  data
}
