#' NA-filled diff
#'
#' Applies the diff function but pads it with \code{NA}s to retain original length.
#'
#' When the gap is an odd number the extra \code{NA} goes at the start.
#'
#' @param x a vector of any type \code{diff} is happy with
#' @param gap the gap to be passed to \code{diff}
#'
#' @return A difference vector with \code{NA}s introduced to make it up to the same size as the input
#' @export
#'
filldiff<-function(x,gap=1) {
  x<-c(rep(NA,ceiling(gap/2)),diff(x,gap),rep(NA,floor(gap/2)))
  return(x)
}
