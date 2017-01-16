#' Track social interaction annotator
#'
#' Calculates closest other fly for each fly and timepoint. Currently obscenely slow.
#'
#' The function loops through each frame, calculates an (n x n-1) distance matrix and looks for value and index of minima.
#' There may be slightly more efficient ways of doing this.
#'
#' @param trx a track data frame
#'
#' @return A data frame containing, for each fly (\code{id}) and timepoint (\code{time}),
#' the distance (\code{closest}) and identity (\code{bff}) of the closest conspecific.
#'
#' @export

trxfriends <- function (trx) {
   timepoints<-unique(trx$time)
   friends <- NULL
   for (t in 1:length(timepoints)) {
      now <- trx[trx$time==t,c('id','x','y','time')]
      xdist <- dist(now$x,upper=T)
      ydist <- dist(now$y,upper=T)
      flydist <- as.matrix(sqrt(xdist^2+ydist^2))
      diag(flydist) <- NA
      closest <- apply(flydist,2,min,na.rm=T)
      bff <- apply(flydist,2,which.min)
      nowfriends <- data.frame ("time" = now$time,"id" = now$id,closest,"bff" = now$id[bff])
      friends<-rbind(friends,nowfriends)
   }
   return(friends)
}
