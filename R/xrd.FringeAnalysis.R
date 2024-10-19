#' XRD / XRR Fringe Analysis
#'
#' @description
#' Searches for fringes in the XRD or XRR spectrum and
#' returns a list with the thickness, a peak table, and
#' two graphs displaying the results.
#'
#'
#' @param df data frame with theta (2q) and I (intensity)
#'
#' @importFrom stats loess
#' @importFrom rlang .data
#' @importFrom ggplot2 aes ggplot geom_point geom_vline scale_y_log10 ylab xlab theme_bw geom_smooth scale_x_continuous
#'
#' @returns list with thickness, table of peaks, and two ggplot graphs
#' @author Thomas Gredig
#'
#' @examples
#'   filename <- xrd.getSampleFiles('rasx')
#'   data <- xrd.import(filename)
#'   analysis <- xrd.FringeAnalysis(data)
#'   print(analysis$t.nm)
#'
#' @export
xrd.FringeAnalysis <- function(df) {
  # smoothen function
  lo <- loess(data = df,
              I ~ theta,
              span = round(20/nrow(df),2))
  # find derivative
  df$dI.lo = c(0, diff(predict(lo)))
  df$peak.loc = c(0, diff(sign(df$dI.lo)))

  peaks = df$theta[which(df$peak.loc < 0)][-1]

  g1 <- df %>%
    ggplot(aes(.data$theta, I)) +
    geom_point(col='blue') +
    geom_vline(xintercept = peaks, col='red') +
    scale_y_log10() + xlab('2\U03B8 (\U00B0)') + ylab("I (a.u.)") +
    theme_bw()

  df.peak = data.frame(
    n = 1:length(peaks),
    th.pos = peaks
  )

  g2 <- df.peak %>%
    ggplot(aes(n, .data$th.pos)) +
    geom_smooth(alpha=0.5, col='grey') +
    geom_point(col='black', size=4) +
    geom_point(col='red', size=3) +
    scale_x_continuous(breaks=0:20*2) +
    xlab("n") + ylab('Peak 2\U03B8 (\U00B0)') +
    theme_bw()

  # find thicknesses
  t.nm = c()
  for(n in 1:(nrow(df.peak)-1)) {
    t.nm = c(t.nm, xrd.FringeThickness(df.peak$th.pos[n]/2,df.peak$th.pos[n+1]/2)/10)
  }


  list(t.nm = t.nm,
       df.peak = df.peak,
       g.peaks = g1,
       g.thickness = g2)
}


