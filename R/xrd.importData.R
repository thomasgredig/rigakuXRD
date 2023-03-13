#' Imports Rigaku XRD Data File
#'
#' Import function recognizes ASC, TXT, RAS, and RASX files from Rigaku XRD
#' instrument; returns dataframe with 2theta, I (intensity normalized for time),
#' and I.meas (measured intentsity)
#'
#' @param filename full file name with path
#' @return data frame with XRD data
#'
#' @author Thomas Gredig
#' @examples
#'
#' fname = xrd.getSampleFiles()[1]
#' d = xrd.import(fname)
#' plot(d$theta,d$I,log='y',col='red')
#'
#' @importFrom tools file_ext
#' @export
xrd.import <- function(filename) {
  fileExtension = tolower(file_ext(filename))
  d <- data.frame()

  if (!file.exists(filename)) {
    warning(paste("File:",filename,"not found. Cannot import XRD data."))
    return(d)
  }

  if (fileExtension=='asc')       d=xrd.read.ASC(filename)
  else if (fileExtension=='ras')  d=xrd.read.RAS(filename)
  else if (fileExtension=='txt')  d=xrd.read.TXT(filename)
  else if (fileExtension=='rasx') d=xrd.read.RASX(filename)
  else warning('File extension is not recognized; cannot read the file.')
  d
}



# ==========================
# ASC XRD Reader: preferred
# ==========================



#' Reads the ASC Rigaku XRD file with a Header
#' @param filename filename including path
#' @return data frame with XRD data
#' @examples
#'
#' fname = xrd.getSampleFiles('asc')[1]
#' d = xrd.read.ASC(file.path(fname))
#' plot(d$theta,d$I,log='y',col='red')
#'
#' @importFrom stringr str_extract_all
#' @importFrom utils read.csv
#' @importFrom stats na.omit
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
    I = x2[x2p:(x2p+length(theta)-1)]
    data = rbind(data, cbind(theta=theta, I = I/norm[num], I.meas = I, loop=num))
    x2p = x2p + length(theta)
  }
  data
}



#' Reads the header of an XRD ASC file
#' @param filename filename including path
#' @return data frame with XRD header
#' @examples
#' d = xrd.readHeader.ASC(xrd.getSampleFiles('asc')[1])
#' head(d)
#'
#' @export
xrd.readHeader.ASC <- function(filename) {
  if(file.exists(filename)==FALSE) { warning(paste('File does not exist:',filename)) }
  data = read.csv(file=filename, stringsAsFactors=FALSE, row.names=NULL, header=FALSE)

  # get data only
  con.File = file(filename, "r")
  hdata = readLines(con.File)
  close(con.File)
  hdata <- hdata[grep("^\\*.*\\t", hdata)]
  hdata <- gsub('\\t+','=',hdata)
  f <- strsplit(hdata,'==')
  d = data.frame(
    name = sapply(f, '[[',1),
    val = sapply(f, '[[',2)
  )
  d$name = gsub('^\\*','',d$name)
  d$name = gsub('_',' ',d$name)
  d$name = tolower(d$name)
  d$val = gsub('^.*=\\s*','',d$val)

  d
}


# ==========================
# RAS XRD Reader
# ==========================



#' Reads the RAS Rigaku XRD data
#' @param filename filename including path
#' @return data frame with XRD data
#' @examples
#'
#' fname = xrd.getSampleFiles('ras')[1]
#' d = xrd.read.RAS(fname)
#' plot(d$X2.Theta,d$I,log='y',col='red')
#'
#' @importFrom utils read.csv
#' @importFrom tidyr "%>%"  separate_wider_delim
#'
#' @export
xrd.read.RAS <- function(filename) {
  if(file.exists(filename)==FALSE) { warning(paste('File does not exist:',filename)) }
  data = read.csv(file=filename, stringsAsFactors=FALSE, row.names=NULL)
  p = as.vector(unlist(data))
  if(p[1]!="*RAS_HEADER_START") { warning(paste("File format is not RAS:",filename))}
  p.start = grep('*RAS_INT_START',p)
  p.end = grep('*RAS_INT_END',p)
  d1 = data.frame(
    n = p[(p.start+1):(p.end-1)],
    stringsAsFactors = FALSE
  )

  lines.comment = grep('^\\*',p)
  n = gsub('^\\*','',p[lines.comment])
  n = n[grepl(" ",n)]
  d.header = data.frame(n, stringsAsFactors = FALSE) %>%
    separate_wider_delim(n, names=c('name','value'), delim=" ", too_many="merge")
  label.x = .xrdRasHeaderValue(d.header,'DISP_TAB_NAME')
  label.y = .xrdRasHeaderValue(d.header,'DISP_TITLE_Y')
  label.units = .xrdRasHeaderValue(d.header,'DISP_UNIT_Y')

  d1 %>%
    separate_wider_delim(n, names=c(label.x,label.y,label.units),
                         delim=" ", too_many="merge") %>%
    lapply(as.numeric) -> d2
  d3 = as.data.frame(d2)
}




.xrdRasHeaderValue <- function(d,item='FILE_MEMO') {
  d$value[grep(item,d$name)]
}


# ==========================
# TXT XRD Reader
# ==========================



#' Reads the TXT Rigaku XRD file with a Header
#' @param filename filename including path
#' @return data frame with XRD data
#' @examples
#'
#' fname = xrd.getSampleFiles('txt')
#' d = xrd.read.TXT(fname)
#' plot(d$theta,d$I,log='y',col='red')
#'
#' @importFrom utils read.csv
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
  if (is.na(start.row)) { d = xrd.read.TXTnoheader(filename) }
  else {
    p1 = strsplit(p[start.row:length(p)],'\t')
    d = data.frame(
      v1 = as.numeric(unlist(lapply(p1,'[[',1))),
      v2 = as.numeric(unlist(lapply(p1, secondValue)))
    )
    names(d) = col.names
  }

  # change column names
  names(d) = c('theta','I')
  d$I.meas = d$I
  d$loop = 1
  d
}


#' Reads the TXT Rigaku XRD file with no header
#' @param filename filename including path
#' @return data frame with XRD data and columns TwoTheta and I
#' @examples
#'
#' fname = xrd.getSampleFiles('txt')[1]
#' d = xrd.read.TXTnoheader(fname)
#' plot(d$TwoTheta,d$I,log='y',col='red')
#'
#' @importFrom utils read.csv
#' @export
xrd.read.TXTnoheader <- function(filename) {
  if(file.exists(filename)==FALSE) { warning(paste('File does not exist:',filename)) }
  d = read.csv(file=filename, sep='\t', stringsAsFactors=FALSE, header=FALSE)
  #q1 = c()
  names(d) = c('TwoTheta','I')
  d
}




# ==========================
# RASX XRD Reader
# ==========================



#' Reads the RASX Rigaku XRD data
#' @param filename filename including path
#' @return data frame with XRD data
#'
#' @author Thomas Gredig
#' @examples
#' fname = xrd.getSampleFiles('rasx')[1]
#' d = xrd.read.RASX(fname)
#' head(d)
#'
#' @importFrom utils read.csv unzip
#'
#' @export
xrd.read.RASX <- function(filename) {
  if (!file.exists(filename)) { stop(paste('File does not exist:',filename)) }

  pTemp = tempdir()
  unzip(filename, exdir=pTemp)
  dataFile = file.path(pTemp, 'Data0', 'Profile0.txt')

  data = read.csv(file=dataFile, sep='\t', stringsAsFactors=FALSE, row.names=NULL, header=FALSE)
  names(data) = c('theta','I.meas','num')

  # change columns
  data = data.frame(
    theta = data$theta,
    I = data$I.meas,
    I.meas = data$I.meas,
    loop = data$num
  )
  data
}

