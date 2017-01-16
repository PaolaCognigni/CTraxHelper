#' Track event classifier
#'
#' Uses speed and location information to classify entries into \code{stand}, \code{run} or \code{edge} events.
#'
#' The classification is based on the columns \code{speed} (output of \code{flymoves} or \code{safemoves}) and \code{radius} (output of \code{trxgarnish}).
#' If these are not already included, they will be calculated ad hoc. Rows with NAs (with the exception of \code{turn}, which is tolerated as it is not used for calculations but has more NAs than anything else)
#' are removed, then each point is categorised as either edge or not (by a threshold pixel distance \code{edge})
#' and either run or not (by a threshold pixel/frame speed \code{sep}). Short running/standing bouts are eliminated
#' by morphological opening and closing over a window of \code{morph} frames. Then, each entry is categorised as either
#' \code{edge}, \code{run} or \code{stand} in the classifier variable \code{whatis} - note that there is currently no distinction between
#' runs and stands in the edge region.
#'
#' Note: currently the opening/closing is not run by id, so there is a bit of spillage
#' between consecutive tracks, but it should only affect ~4 frames on each side. It would still be best to run the function via ave.
#'
#'
#'
#' @param trx a track data frame
#' @param sep the discriminant speed value between runs and stands. Set at e^-1 on the basis of eyeballed data.
#' @param edge the number of pixels from the rim of the arena to be categorised as edge.
#' @param morph the window for morphological operations
#' @param runlag the \code{framelag} passed to \code{flyrun} in case the speed column is missing
#'
#' @return a track data frame that includes the boolean columns \code{isedge} and \code{isrun} and the factor column \code{whatis}
#'
#' @export

flyclass <- function (trx,sep=exp(-1),edge=12,morph=7,runlag=6)
{

  # check that appropriate radius data is available, if not generate it
  if (!("radius" %in% colnames(trx))) {
    trx<-trxgarnish(trx)
  }

  # check that appropriate speed data is available, if not generate it
  if (!("speed" %in% colnames(trx))) {
    trx<-cbind(trx,"speed"=flyrun(trx,runlag))
  }

  # set aside rows with NAs and use only complete rows. NAs in turn are not important
  # since these values are not used for any calculation at the moment, so entries that have NAs
  # in turn only are kept
  fulltrx<-trx
  goodrows<-complete.cases(trx[,(colnames(trx)!="turn")])
  trx<-trx[goodrows,]

  # select flies within edge of the max radius, then open/close the values
  maxr<-max(trx$radius[trx$radius<400])
  isedge<-trx$radius>(maxr-edge)
  isedge<-mmand::closing(mmand::opening(as.numeric(isedge),rep(1,morph)),rep(1,morph))

  # select fly state by speed then open/close
  isrun<-trx$speed>sep
  isrun<-mmand::closing(mmand::opening(as.numeric(isrun),rep(1,morph)),rep(1,morph))
  whatis<-isrun+isedge*2

  # add the appropriate columns to the original df and fill them with NAs
  fulltrx<-cbind(fulltrx,'isedge'=NA,'isrun'=NA,'whatis'=NA)
  fulltrx$isedge[goodrows]<-isedge
  fulltrx$isrun[goodrows]<-isrun
  fulltrx$whatis[goodrows]<-whatis

  # factorise the classifier and collapse all edge values into one class
  fulltrx$whatis<-factor(fulltrx$whatis)
  levels(fulltrx$whatis)<-c('stand','run','edge','edge')
  return(fulltrx)
}
