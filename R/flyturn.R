#' Track angular speed calculator
#'
#' Calculates the angular speed in rad/frame by comparing the angle of velocity vectors calculated across \code{framelag}.
#'
#' For each timepoint t the speed vector is calculated between \code{t-framelag} and \code{t}, and then between \code{t} and \code{t+framelag}.
#' The angles are extracted from these two vectors and subtracted to yield the change in angle over \code{timeframe}.
#' This creates a lot of problems when the angles are close to the discontinuity of 2pi and needs sorting out.
#' Currently this leaves \code{framelag NA}s at the beginning and end of each track. This causes all sorts of problems
#' and may be best addressed somehow in the future.
#'
#' Realistic data observation suggests that any angles over pi/2 are either artefacts or discontinuity errors, but at the moment they are not \code{NA}'d.
#'
#' @param trx a track data frame (only requires CTrax output variables)
#' @param framelag the number of frames to derive across; defaults to 6
#'
#' @return A numeric vector matching the structure of \code{trx} containing the angular speed value for each object and timepoint, or \code{NA}s when it can't be calculated.
#'
#' @export

flyturn <- function (trx, framelag = 6)
{
  dx1<- ave(trx$x,trx$id,FUN=function(a)
    c(diff(a,framelag),rep(NA,framelag)))
  dy1<- ave(trx$y,trx$id,FUN=function(a)
    c(diff(a,framelag),rep(NA,framelag)))
  angle1<-atan2(dx1,dy1)
  dx2<- ave(trx$x,trx$id,FUN=function(a)
    c(rep(NA,framelag),diff(a,framelag)))
  dy2<- ave(trx$y,trx$id,FUN=function(a)
    c(rep(NA,framelag),diff(a,framelag)))
  angle2<-atan2(dx2,dy2)
  turn<-angle2-angle1
  return(turn)
}
