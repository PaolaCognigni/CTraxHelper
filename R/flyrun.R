#' Track speed calculator
#'
#' Calculates the speed (scalar) in px/frame by comparing centroid location across \code{framelag}.
#'
#' For each timepoint t the velocity vector is calculated between \code{t-framelag/2} and \code{t+framelag+2}.
#' Currently this leaves \code{framelag/2 NA}s at the beginning and end of each track. This causes all sorts of problems
#' and may be best addressed somehow in the future.
#'
#'  Frames with \code{speed}>50 are also currently \code{NA}'d since that is exclusively artifacts (CTrax already limits jumps to 40 px/frame)
#'  however this again causes holes in the data which become problematic later. Ideally the imputation would be done at the \code{x} and \code{y} level.
#'
#' @param trx a track data frame (only requires CTrax output variables)
#' @param framelag the number of frames to derive speed across; defaults to 6
#'
#' @return A numeric vector matching the structure of \code{trx} containing the speed value for each object and timepoint, or \code{NA}s when it can't be calculated.
#'
#' @export

flyrun <- function (trx, framelag=6)
{
  dx<- ave(trx$x,trx$id,FUN=function(a)
    c(rep(NA,floor(framelag/2)),diff(a,framelag),rep(NA,ceiling(framelag/2))))
  dy<- ave(trx$y,trx$id,FUN=function(a)
    c(rep(NA,floor(framelag/2)),diff(a,framelag),rep(NA,ceiling(framelag/2))))
  speed<-((dx^2+dy^2)^0.5)/framelag
  speed[speed>50]<-NA
  return(speed)
}
