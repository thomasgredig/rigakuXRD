read.xrdasc.info <- function(filename) {
  if(file.exists(filename)==FALSE) { warning(paste('file does not exist:',filename)) }
  data = read.csv(file=filename, stringsAsFactors=FALSE)
  as.vector(unlist(data))->p
  num.lines = length(p)
  str_extract_all(p, '\\*{1}SAMPLE\t') -> a
  which(a=="*SAMPLE\t")->q
  strsplit(p[q],'=')[[1]][2] -> sample.name
  strsplit(p[q+3],'=')[[1]][2] -> sample.date
  str_extract_all(p, '\\*{1}MEMO\t') -> a
  which(a=="*MEMO\t")->q
  strsplit(p[q],'=')[[1]][2] -> sample.memo
  str_extract_all(p, '\\*{1}COUNT\t') -> a
  which(a=="*COUNT\t")->q

  data.frame(gsub("^\\s*",'',cbind(name=sample.name, date=sample.date, memo=sample.memo, loops=length(q), data.lines=num.lines )))
}

make.filelist.asc <- function(path) {
  result=data.frame()
  dir(path=path, pattern='\\.asc$', recursive=TRUE) -> file.list
  for (m in file.list) {
    file=paste(path,m,sep='/')
    info = read.xrdasc.info(file)
    result=rbind(result, cbind(file=m, path=path, info))
  }
  result
}

xrd.plot <- function(file.list, num, loop.num) {
  file.xrd = paste(file.list$path[num],file.list$file[num],sep='/')
  data = read.xrdasc(file.xrd)
  d=subset(data, loop==loop.num)
  ggplot(d, aes(theta, I, color=as.factor(loop))) + geom_point(size=4) +
    theme_tg() + scale_y_log10() +
    xlab(expression(paste('2',theta))) +
    ggtitle(info$name) -> m
  m
}

xrd.plot.overlap <- function(file.list, num.list, range, alpha=100) {
  result = data.frame()
  intentsity.offset = 0
  for (num in num.list) {
    file.xrd = paste(file.list$path[num],file.list$file[num],sep='/')
    data = read.xrdasc(file.xrd)
    d = subset(data, theta>=range[1] & theta<=range[2])
    d$I=d$I + intentsity.offset
    intentsity.offset = intentsity.offset + alpha
    d$sample = rep(file.list$name[num],nrow(d))
    result=rbind(result,d)
  }
  ggplot(result, aes(theta, I, color=sample)) + geom_line(size=0.6) +
    theme_tg(base_size=10,legend.size=0.3) + scale_y_log10() +
    xlab(expression(paste('2',theta))) -> m
  m
}

xrd.get.filename <- function(num) {
  paste(file.list$path[num],file.list$file[num],sep='/')
}
