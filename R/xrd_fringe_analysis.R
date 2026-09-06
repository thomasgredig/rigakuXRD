#' XRD / XRR fringe analysis
#'
#' @description
#' Detects interference fringes in an XRD or XRR spectrum and returns a
#' list containing the estimated film thickness, a peak table, and two
#' diagnostic plots.
#'
#' @param dataXRD A data frame or \code{xrd} object containing \code{TwoTheta}
#'   (2theta, in degrees) and intensity values (\code{I}).
#'
#' @return A list with the following components:
#' \describe{
#'   \item{\code{t.nm}}{Estimated thickness in nm.}
#'   \item{\code{df.peak}}{Data frame of detected fringe peak positions.}
#'   \item{\code{g.peaks}}{ggplot showing detected peaks.}
#'   \item{\code{g.thickness}}{ggplot summarizing the thickness estimate.}
#' }
#'
#' @author Thomas Gredig
#'
#' @examples
#' filename <- xrd_sample_files("ras")
#' data <- xrd_import(filename, xrd = TRUE)
#' data_5 <- xrd_filter(data, 5, 10)
#' analysis <- xrd_fringe_analysis(data_5)
#' analysis$t.nm
#'
#' @importFrom stats loess predict
#' @importFrom rlang .data
#' @importFrom ggplot2 aes ggplot geom_point geom_vline scale_y_log10 ylab xlab theme_bw geom_smooth scale_x_continuous
#'
#' @export
xrd_fringe_analysis <- function(dataXRD) {
  df <- check_dataXRD(dataXRD)
  lo <- loess(data = df,
              I ~ TwoTheta,
              span = round(20/nrow(df),2))
  df$dI.lo = c(0, diff(predict(lo)))
  df$peak.loc = c(0, diff(sign(df$dI.lo)))

  peaks = df$TwoTheta[which(df$peak.loc < 0)][-1]

  g1 <- df %>%
    ggplot(aes(.data$TwoTheta, I)) +
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

  t.nm = c()
  for(n in 1:(nrow(df.peak)-1)) {
    t.nm = c(t.nm, xrd_fringe_thickness(df.peak$th.pos[n]/2,df.peak$th.pos[n+1]/2)/10)
  }

  list(t.nm = t.nm,
       df.peak = df.peak,
       g.peaks = g1,
       g.thickness = g2)
}


#' @rdname xrd_fringe_analysis
#' @export
xrd.FringeAnalysis <- function(dataXRD) {
  .Deprecated("xrd_fringe_analysis", package = "rigakuXRD")
  xrd_fringe_analysis(dataXRD)
}
