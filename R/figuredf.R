#' Figure reshaping for plotting purposes
#'
#' Generates a plot-friendly data frame from the list output of \code{flyfigures}.
#'
#' Requires the list format to map each figure to a specific fly (\code{id}) and moment (\code{starttime}).
#' If the optional argument \code{trx} is also passed, all the other attributes attached to trx are also
#' included by merging.
#'
#' @param figures a figure list, output of \code{flyfigures}
#' @param trx a track data frame
#'
#' @return a data frame including \code{x} and \code{y} values stacked by \code{start} (figure start time: constant for each figure
#' and consistent with experimental frame) and \code{step} (counter from first to last frame of the figure; \code{start} + \code{step}
#' should reconstruct data point frame), and labelled with \code{id}.
#'
#' @export
#'
#'
figuredf<-function (fig) {
  if (mode(fig)=="list" & allc("x","y","id","start") %in% (names(fig))) {
    stepsize = ncol(fig$x)
    nfigs = nrow(fig$x)
    steps<-data.frame(
      'x'=matrix(fig$x,ncol=1),
      'y'=matrix(fig$y,ncol=1),
      'step'=rep(0:(stepsize-1),each=nfigs),
      'starttime'=rep(fig$start,stepsize),
      'id'=rep(fig$id,stepsize) )
  steps<-steps[order(steps$id,steps$starttime),]
  return(steps)
  }
  else {stop(print("Wrong format"))}
}
