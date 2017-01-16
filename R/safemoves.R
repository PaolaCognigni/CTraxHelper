#' Track motion parameter calculator
#'
#' A wrapper function that calls \code{\link{flyrun}}, \code{\link{flyangle}} and \code{\link{flyturn}} and appends them to a track data frame, then removes any entry containing even one \code{NA}. Used for follow-up analysis that can't handle NAs.
#'
#' Each motion function can receive a specific \code{framelag} value; by default they are all at 6.
#' This function is kept separate from \code{\link{trxgarnish}} so that it can be run with different \code{framelag} parameters without recalculating all the location variables as well.
#'
#' @param trx a track data frame (CTrax values)
#' @param speedlag frame lag for the calculation of speed
#' @param anglelag frame lag for the calculation of the velocity vector angle
#' @param turnlag frame lag for the calculation of the angular velocity
#'
#' @return A track data frame including the columns \code{speed}, \code{angle} and \code{turn}. Every \code{NA} entry will be removed.
#' The overwhelming majority of these will be \code{framelag} chunks at the beginning and end of tracks (where speed can't be calculated
#' because of lack of hindsight information) which don't affect the data structure much, but some would be artefactual entries culled by
#' the \code{speed>50} or \code{radius<400} sanity check. These could leave holes in the middle of tracks whereby consecutive rows would not be consecutive frames.
#'
#' @export

safemoves <- function (trx,speedlag=6,anglelag=6,turnlag=6)
{
  speed<-flyrun(trx,speedlag)
  angle<-flyangle(trx,anglelag)
  turn<-flyturn(trx,turnlag)
  trx<-cbind(trx,speed,angle,turn)
  trx$radius[trx$radius>400]<-NA
  trx<-trx[complete.cases(trx),]
  return(trx)
}
