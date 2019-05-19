

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
