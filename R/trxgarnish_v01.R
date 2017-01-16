#' Track garnisher for time and position measures - OUTDATED
#'
#' Computes distance from centre, quadrant edge and quadrant identity. If a time protocol is available,
#' it also includes protocol structure information and quadrant preference.
#'
#' Input can be either a \code{trx} data frame or an experiment list containing a \code{trx} data frame
#' and an \code{exp} list of experimental parameters. In the latter case some info is added
#' to the list, namely the arena parameters (in \code{$exp$arena}) and, if passed separately, time protocol (\code{$t}).
#'
#' A time protocol can be passed to the function as an optional var or included in the original list
#' as an element named \code{$t}. It is merged into the track data frame by \code{id} and \code{time}.
#' If the time protocol contains a \code{section} column, this is combined with quadrant information to
#' compute \code{preference} for lit quadrant (assumed to be positive diagonal for section 4 and
#' negative diagonal for section 5).
#'
#' @param explist a track data frame or experiment list (containing a \code{trx} data frame element)
#' @param t an optional time protocol data frame. It must have a \code{time} and an \code{id} column
#' which must match equivalent columns in the track data frame.
#'
#' @return A garnished track data frame or experiment list (depending on the format in input) including
#' the columns \code{radius}, \code{distance} and \code{quadrant}; if there is a time protocol, also
#' includes all columns of that data frame and the \code{preference} column calculated for the appropriate protocol section.
#'
#'

trxgarnish_v01 <- function (explist,t=NULL)
{
  if ("trx" %in% names(explist)) {
    trx<-explist$trx }

  ### guessarena: estimate centre by averaging the tom and bottom x and y values
  ### uses the median of the top and bottom 4 values to minimise outlier/error effects
  arena<-list()
  tempsort<-sort(trx$x)

  ## some tracks are temporarily lost and they are given a x,y location of 0,0; removed here
  ## to ensure they are not computed into the arena estimation
  tempsort<-tempsort[tempsort>1]
  topx<-tail(tempsort,4)
  bottomx<-head(tempsort,4)
  arena$x<-mean(c(median(topx),median(bottomx)))
  tempsort<-sort(trx$y)
  tempsort<-tempsort[tempsort>1]
  topy<-tail(tempsort,4)
  bottomy<-head(tempsort,4)
  arena$y<-mean(c(median(topy),median(bottomy)))
  arena$r<-mean(c(topx-arena$x,arena$x-bottomx,topy-arena$y,arena$y-bottomy))
  if ("exp" %in% names(explist)) {
    explist$exp$arena<-arena }

  ### arena-based localisation variables
  radius<-((trx$x-arena$x)^2+(trx$y-arena$y)^2)^0.5
  distance<-apply(cbind(abs(trx$x-arena$x),abs(trx$y-arena$y)),1,min)
  quadrant<-sign((trx$x-arena$x)*(trx$y-arena$y))
  trx<-cbind(trx,radius,distance,quadrant)

  ### movement variables: not here as it would be a waste to recalculate everything else
  ### every time you want to try a different lag.

  ### time protocol merge - instead of passing it as a separate var,
  ### load it into the exp list with the name t
  if ("t" %in% names(explist)) {
    trx<-merge(trx,explist$t,by="time",all.x=T) }

  ### or if it's easier to pass it to the reader function, do it here
  if (!is.null(t)) {
    trx<-merge(trx,t,by="time",all.x=T) }

  ### if time protocol information has been passed, calculate quadrant preference
  if ("section" %in% colnames(trx)) {
    preference<-trx$distance*trx$quadrant*((trx$section>3)*((trx$section%%2)*2-1))
    trx<-cbind(trx,preference) }

  ## new in version 0.3: order data by id so that gap-based calculations run correctly
  ## (consecutive rows are consecutive frames of one individual)
  ## also collects speed and angular displacement NAs at beginning and end of track
  ## this may become unnecessary if I sort out those values to have better placeholders

  trx<-trx[order(trx$id,trx$time),]

  if ("trx" %in% names(explist)) {
    explist$trx<-trx
    if (!(("t" %in% names(explist)|(is.null(t))))) {
      explist$t<-t  }
  }
  else { explist<-trx }

  return (explist)
}
