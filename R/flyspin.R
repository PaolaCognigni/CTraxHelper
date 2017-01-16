#' Fly body turn calculator
#'
#' Calculates the change in body angle between consecutive frames.
#'
#' Uses the major axis angle of the ellipse fit by CTrax (\code{theta}) as this
#' turns out to be much more reliable than the angular speed approach based on centroid
#' location, especially when there is little movement.
#'
#' Assumes that flies don't turn more than 90 degrees, so 'flips' the fly if that results in a lower angle.
#' Note that this assumption is probably true on a frame-by-frame basis; therefore if \code{framelag} is set
#' to a value >1, it is calculated as the sum of consecutive (small) spins.
#'
#' It is currently run across the whole dataset without splitting it by \code{id}, as the amount of artefacts should
#' be small, but ideally it would be best to run it with an \code{ave}-based design.
#'
#' @param trx a track data frame (only requires CTrax output variables)
#' @param framelag the number of frames to compare angles across; defaults to 1 (consecutive frames)
#'
#' @return A numeric vector matching the structure of \code{trx} containing the spin value for each object and timepoint, or \code{NA}s when it can't be calculated.
#'
#' @export
#'
#'

flyspin <- function (trx, framelag=1) {
  a<-diff(trx$theta)
  a<-anglefix(a)
  a[a>(pi/2)]<-pi-a[a>(pi/2)]
  a[a<(-pi/2)]<-a[a<(-pi/2)]+pi
  b<-rep(NA,length(a))
  for (i in 1:(length(a)-framelag+1)) {
    b[i]<-sum(a[i]:a[i+framelag-1])
  }
  return(c(rep(NA,framelag),b))
}
