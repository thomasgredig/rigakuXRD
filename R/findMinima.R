#' find the "minima" in data
#'
#' @param df data frame with x and y
#'
#' @importFrom npreg ss
#' @export
# findLocalMinimum <- function(df) {
#   mod.ss <- ss(df$x, df$y, nknots = 4)
#   plot(df, log='y')
#   plot(mod.ss)
#   ddf = data.frame(x = mod.ss$x[-1], y = diff(mod.ss$y))
#   plot(ddf)
#   d2df = data.frame(x = ddf$x[-nrow(ddf)], y = diff(ddf$y))
#   plot(d2df)
#   abline(h=0)
#   # cut off the ends
#   nCut = ceiling(nrow(d2df)*0.1)
#   d1 = d2df[nCut:(nrow(d2df)-nCut*2),]
#   d1$sgn = sign(d1$y)
#   d1$x[which(d1$sgn==-1)[1]-1]
# }
