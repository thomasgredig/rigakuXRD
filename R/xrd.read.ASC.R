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
  data = read.csv(file=filename, stringsAsFactors=FALSE, row.names=NULL, header=FALSE)

  # get data only
  xrd.data = read.csv(file=filename, stringsAsFactors=FALSE, row.names=NULL, comment.char = '*', header = FALSE)
  x1 = c(xrd.data$V1, xrd.data$V2, xrd.data$V3, xrd.data$V4)
  ln = nrow(xrd.data)
  # now re-arrange: 1,ln+1, 2ln+1, 3ln+1 , 2, ln+2 , ...
  x1p = rep(1:ln, each=4)
  x1p[(1:ln)*4-2] = x1p[(1:ln)*4-2] + ln
  x1p[(1:ln)*4-1] = x1p[(1:ln)*4-1] + ln*2
  x1p[(1:ln)*4] = x1p[(1:ln)*4] + ln*3
  x2 = x1[x1p]
  na.omit(x2) -> x2

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
  x2p = 1
  for(num in 1:length(q)) {
    theta = seq(from=theta.start[num], to=theta.stop[num], by=theta.step[num])
    I = x2[x2p:(x2p+length(theta))]
    data = rbind(data, cbind(theta=theta, I = I/norm[num], I.meas = I, loop=num))
    x2p = x2p + length(theta)
  }
  data
}
