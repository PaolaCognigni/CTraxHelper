#' Transition matrix generation from event series
#'
#' Takes a vector of consecutive events and compares each to the event \code{chainlag} frames away.
#'
#' This is a bit of a wrapper function because the format of \code{table} is really awkward and I expect I would forget
#' how to handle it and have to re-google it avey time.
#'
#' @param events a vector of consecutive events (for example, the \code{whatis} column of a \code{flyclass}-processed track data frame)
#' @param chainlag frame lag across which to compare
#'
#' @return
#' A transition matrix, normalised to have rows summing to 1 for further probability work.
#'
#'@export
flychain <- function (events,chainlag)
{
  transition<-table(factor(c(events,rep(NA,chainlag))),factor(c(rep(NA,chainlag),events)))
  transition<-as.data.frame.matrix(transition)
  transition<-as.matrix(transition)
  transition<-transition/rowSums(transition)
  return(transition)
}
