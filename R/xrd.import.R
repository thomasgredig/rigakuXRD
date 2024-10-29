#' Imports Rigaku x-ray diffraction data
#'
#' @description
#'
#' Import function recognizes ASC, TXT, RAS, and RASX files from Rigaku x-ray diffraction
#' instrument; returns an xrd object or data frame with 2theta,
#' I (intensity normalized for time) and I.meas (measured intensity).
#'
#' @param filename full file name with path
#' @param xrd logical, if \code{TRUE} an xrd object is returned
#'
#' @return xrd object or data frame with XRD data
#'
#' @author Thomas Gredig
#' @examples
#' fname = xrd.getSampleFiles('rasx')
#' d = xrd.import(fname, TRUE)
#' plot(d)
#'
#' @importFrom tools file_ext
#' @importFrom cli cli_warn
#' @export
xrd.import <- function(filename, xrd = FALSE) {
  check_file_exists(filename)

  fileExtension = tolower(file_ext(filename))
  d <- data.frame()

  d <- switch(fileExtension,
              'asc' = xrd_read_ASC(filename),
              'ras' = xrd_read_RAS(filename),
              'txt' = xrd_read_TXT(filename),
              'rasx' = xrd_read_RASX(filename),
              cli_warn('XRD file extension ({fileExtension}) is not recognized; file cannot be imported.')
  )

  if (xrd) class(d) <- "xrd"
  d
}

#' @importFrom cli cli_abort
#' @noRd
check_file_exists <- function(filename) {
  if (!file.exists(filename)) {
    cli_abort("File {filename} does not exist.")
  }
}

#' Plots xrd class
#'
#' @param x xrd S3 object
#' @param ... Optional graphical parameters to adjust different components of the performance plot
#' @export
plot.xrd <- function(x, ...) {
  plot(x$TwoTheta, x$I, log='y', col='red', xlab='2q', ylab="log I (cps)", ...)
}


# ==========================
# ASC XRD Reader: preferred
# ==========================



#' Reads the ASC Rigaku XRD file with a Header
#' @param filename filename including path
#' @return data frame with XRD data
#' @importFrom stringr str_extract_all
#' @importFrom utils read.csv
#' @importFrom stats na.omit
#' @noRd
xrd_read_ASC <- function(filename) {
  check_file_exists(filename)

  data <- read.csv(file=filename, stringsAsFactors=FALSE, row.names=NULL, header=FALSE)

  # get data only
  xrd.data = read.csv(file=filename, stringsAsFactors=FALSE, row.names=NULL, comment.char = '*', header = FALSE)
  x1 = c(xrd.data$V1, xrd.data$V2, xrd.data$V3, xrd.data$V4)
  ln = nrow(xrd.data)
  # now re-arrange: 1,ln+1, 2ln+1, 3ln+1 , 2, ln+2 , ...
  x1p <- rep(1:ln, each=4)
  x1p[(1:ln)*4-2] <- x1p[(1:ln)*4-2] + ln
  x1p[(1:ln)*4-1] <- x1p[(1:ln)*4-1] + ln*2
  x1p[(1:ln)*4] <- x1p[(1:ln)*4] + ln*3
  x2 <- x1[x1p]
  na.omit(x2) -> x2

  as.vector(unlist(data))->p

  str_extract_all(p, '\\*{1}COUNT\t') -> a
  which(a=="*COUNT\t") -> q
  as.numeric(gsub("\\D", "", p[q]))  -> b

  x.start = q+1
  x.end = q+b

  str_extract_all(p, '\\*{1}START\t') -> angle
  which(angle=="*START\t") -> q

  # Define a function to extract numeric values from the string
  extract_numeric <- function(p, q, seq) {
    as.numeric(unlist(strsplit(p[q], '='))[seq])
  }

  # Initialize sequence
  seq <- 1:(length(q)) * 2

  # Extract values using the function
  theta.start <- extract_numeric(p, q, seq)
  q <- q + 1
  theta.stop <- extract_numeric(p, q, seq)
  q <- q + 1
  theta.step <- extract_numeric(p, q, seq)
  q <- q + 2
  theta.speed <- extract_numeric(p, q, seq)

  # Assign theta.speed to norm
  norm <- theta.speed

  data <- data.frame()
  x2p <- 1
  for(num in 1:length(q)) {
    theta = seq(from=theta.start[num], to=theta.stop[num], by=theta.step[num])
    I <- x2[x2p:(x2p+length(theta)-1)]
    data <- rbind(data, cbind(TwoTheta=theta, I = I/norm[num], I.meas = I, loop=num))
    x2p <- x2p + length(theta)
  }
  data
}



#' Reads the header of an XRD ASC file
#' @param filename filename including path
#' @return data frame with XRD header
#'
#' @export
xrd.readHeader.ASC <- function(filename) {
  check_file_exists(filename)
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
#'
#' @importFrom utils read.csv
#' @importFrom tidyr separate_wider_delim
#'
#' @noRd
xrd_read_RAS <- function(filename) {
  check_file_exists(filename)
  data = read.csv(file=filename, stringsAsFactors=FALSE, row.names=NULL)
  p = as.vector(unlist(data))
  p = iconv(p, from = "ISO-8859-1", to = "UTF-8")

  if(p[1]!="*RAS_HEADER_START") { warning(paste("File format is not RAS:",filename))}
  p.start = grep('*RAS_INT_START',p)
  p.end = grep('*RAS_INT_END',p)
  if (length(p.start)==0L | length(p.end)==0L) { stop("RAS file data start and end not found.") }
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

  d2 <- separate_wider_delim(d1,n, names=c(label.x,label.y,label.units),
                         delim=" ", too_many="merge")
  d3 <- lapply(d2, as.numeric)
  names(d3) <- c("TwoTheta","I","cps")

  d3$I.meas = d3$I / d3$cps
  d3$loop = 1
  d3$cps <- NULL

  as.data.frame(d3)
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
#'
#' @importFrom utils read.csv
#' @noRd
xrd_read_TXT <- function(filename) {
  check_file_exists(filename)
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
  if (is.na(start.row)) { d = xrd_read_TXTnoheader(filename) }
  else {
    if (length(grep('\t', p[start.row]))>0) {
      p1 = strsplit(p[start.row:length(p)],'\t')
    } else {
      # separator is space not tab
      p1 = strsplit(p[start.row:length(p)],' ')
    }
    d = data.frame(
      v1 = as.numeric(unlist(lapply(p1,'[[',1))),
      v2 = as.numeric(unlist(lapply(p1, secondValue)))
    )
    names(d) = col.names
  }

  # change column names
  names(d) = c('TwoTheta','I')
  d$I.meas = d$I
  d$loop = 1
  d
}


#' Reads the TXT Rigaku XRD file with no header
#' @param filename filename including path
#' @return data frame with XRD data and columns TwoTheta and I
#'
#' @importFrom utils read.csv
#' @noRd
xrd_read_TXTnoheader <- function(filename) {
  check_file_exists(filename)
  d = read.csv(file=filename, sep='\t', stringsAsFactors=FALSE, header=FALSE)
  if(ncol(d)!=2) cli_abort("Format of data in XRD file {filename} is not recognized.")
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
#' @importFrom utils read.csv unzip
#'
#' @noRd
xrd_read_RASX <- function(filename) {
  check_file_exists(filename)

  pTemp = tempdir()
  unzip(filename, exdir=pTemp)
  dataFile = file.path(pTemp, 'Data0', 'Profile0.txt')

  data = read.csv(file=dataFile, sep='\t', stringsAsFactors=FALSE, row.names=NULL, header=FALSE)
  names(data) = c('TwoTheta','I.meas','num')

  # change columns
  data = data.frame(
    TwoTheta = data$TwoTheta,
    I = data$I.meas,
    I.meas = data$I.meas,
    loop = data$num
  )
  data
}

