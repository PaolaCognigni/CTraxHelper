#' Track motion parameter calculator
#'
#' A wrapper function that calls \code{\link{flyrun}}, \code{\link{flyangle}} and \code{\link{flyturn}} and appends them to a track data frame.
#'
#' Each motion function can receive a specific \code{framelag} value; by default they are all at 6.
#' This function is kept separate from \code{\link{trxgarnish}} so that it can be run with different \code{framelag} parameters without recalculating all the location variables as well.
#' In the future \code{angle} could be optional as it's a bit of a lame duck variable.
#'
#' @param trx a track data frame (CTrax values)
#' @param speedlag frame lag for the calculation of speed
#' @param anglelag frame lag for the calculation of the velocity vector angle
#' @param turnlag frame lag for the calculation of the angular velocity
#'
#' @return A track data frame including the columns \code{speed}, \code{angle} and \code{turn}. These will contain \code{NA} chunks (longer for \code{angle}).
#'
#' @export
flymoves <- function (trx,speedlag=6,anglelag=6,turnlag=6)
  {
   speed<-flyrun(trx,speedlag)
   angle<-flyangle(trx,anglelag)
   turn<-flyturn(trx,turnlag)
   trx<-cbind(trx,speed,angle,turn)
   return(trx)
  }
