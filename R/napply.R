#' NApply
#'
#' Apply a function raggedly to a vector containing NAs.
#'
#' Each consecutive stretch of non-NA values is treated as a separate input to the function (using \code{ave}).
#' Can be used as a generalised method for functions that don't have a \code{na.omit} option or when
#' it's important to maintain data structure and order.
#'
#' The function is best used for \code{FUN}s which return an output of the same size as the input.
#' If \code{FUN} returns an atomic value for an input vector (eg \code{mean}) the output is replicated across
#' all entries of x as necessary.
#'
#'
#' @param x a vector
#' @param FUN a function
#'
#' @return A vector of the same size and NA distribution as \code{x} but containing \code{FUN}-transformed data.
#' @export
#'

napply<-function(x,FUN) {

  where<-rle(!is.na(x))
  newwhere<-rep(NA,length(where$values))
  newwhere[where$values]<-1:sum(where$values)
  where$values<-newwhere
  sections<-factor(inverse.rle(where))
  x<-ave(x,sections,FUN=FUN)
  return(x)

}
