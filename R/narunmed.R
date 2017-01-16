#' NA-tolerant running median
#'
#' A serialising function that applies the \code{\link{runmed}} function to each consecutive stretch of
#' non-NA values in a numeric vector.
#'
#' @param xin a numeric vector.
#' @param w the (odd numbered) median window to be passed to \code{runmed}.
#'
#' @return
#' A vector with the same size and structure as the original (reconstructed by plonking NAs appropriately)
#' but running median-filtered values.
#'
#' @export
narunmed<-function(xin,w) {
  holes<-rle(is.na(xin))
  xout<-NULL
  for (i in 1:length(holes$values)) {
     if (holes$values[i]) {
        xout<-c(xout,rep(NA,holes$length[i])) }
     else  {
        beg <- sum(holes$lengths[1:i-1])+1
        end <- sum(holes$lengths[1:i])
        if (any(is.na(xin[beg:end]))) {
           print ("There's a problem") }
        else  {
           xout<-c(xout,runmed(xin[beg:end],w)) }
      }
   }
  return (xout)
}





