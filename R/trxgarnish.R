#' Track garnisher for time and position measures
#'
#' Computes distance from centre, quadrant edge and quadrant identity. Frame-by-frame variables are computed
#' (frame-by frame speed (\code{step}), angle relative to plate (\code{sidestep}) and change in theta (\code{spin})).
#' and also used for some quality control (eliminate speed>50). For movement calculated over different lags, see
#' \code{\link{flymoves}}).
#'
#' If a time protocol is available, it also includes protocol structure information and calculates quadrant preference
#' (assuming quandrant patterns in sections 4 and 5 only).
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
#' the columns \code{radius}, \code{edge}, \code{distance} and \code{quadrant}, and the first and second
#' derivative variables \code{step}, \code{sidestep} and \code{spin}; if there is a time protocol, also
#' includes all columns of that data frame and the \code{preference} column calculated for the appropriate protocol section.
#' @export
#'

trxgarnish <- function (explist,t=NULL,filter=TRUE,jump=40)
  {
   if ("trx" %in% names(explist)) {
      trx<-explist$trx }
   else trx<-explist

   ### guessarena: estimate centre by averaging the tom and bottom x and y values
   ### uses the median of the top and bottom 4 values to minimise outlier/error effects
   arena<-list()
   position<-complex(real=trx$x,imaginary=trx$y)

   ## some tracks are temporarily lost and they are given a x,y location near 0,0; removed here
   ## to ensure they are not computed into the arena estimation
   position[Mod(position)<20]<-NA

   ### in case the arena variables are passed or pre-existing somehow, use those
   ### otherwise calculate the location of the centre of the arena based on data

   if (!("arena" %in% names(explist$exp))) {
                                        tempsort<-sort(Re(position))
                                        topx<-tail(tempsort,4)
                                        bottomx<-head(tempsort,4)
                                        arena$x<-mean(c(median(topx),median(bottomx)))
                                        tempsort<-sort(Im(position))
                                        topy<-tail(tempsort,4)
                                        bottomy<-head(tempsort,4)
                                        arena$y<-mean(c(median(topy),median(bottomy)))
                                        arena$r<-max(c(topx-arena$x,arena$x-bottomx,
                                                       topy-arena$y,arena$y-bottomy))
                                       }
   if ("exp" %in% names(explist)) {
                                        explist$exp$arena<-arena }

   ### re-center x,y to the arena centre and REPLACE original location variables
   position<-complex(real=(Re(position)-arena$x),imaginary=(Im(position)-arena$y))
   trx$x<-Re(position)
   trx$y<-Im(position)

   ### calculate polar coordinates and use them to define quadrant identity
   r<-Mod(position)
   edge<-max(r)-r
   phi<-Arg(position)
   quadrant<-as.integer((phi%%pi<(pi/2))*2-1)
   distance<-apply(cbind(abs(trx$x),abs(trx$y)),1,min)
   trx<-cbind(trx,r,edge,phi,distance,quadrant)

   ## order data by id so that gap-based calculations run correctly
   ## (consecutive rows are consecutive frames of one individual)
   trx<-trx[order(trx$id,trx$time),]

   ### movement variables: only frame-to-frame vars are calculated
   dx<-ave(trx$x,trx$id,FUN=filldiff)
   dy<-ave(trx$y,trx$id,FUN=filldiff)
   step<-(dx^2+dy^2)^0.5
   yaw<-anglefix(flyturn(trx,framelag=1))
   yaw<-(abs(yaw)>(pi/2))*pi + yaw*((yaw<=(pi/2))*2-1)
   spin<-anglefix(ave(trx$theta,trx$id,FUN=filldiff))
   spin<-(abs(spin)>(pi/2))*pi + spin*((spin<=(pi/2))*2-1)
   trx<-cbind(trx,step,yaw,spin)

   ## filter option: identifies unrealistic jumps, replaces movement variables with NA's
   if (filter==TRUE) {
        trx$step[trx$step>jump]<-NA
        trx$yaw[trx$step>jump]<-NA
        }

   ### t can be attached to the experiment list as a data frame t (this will override a passed t)
   if ("t" %in% names(explist)) {
      t<-explist$t }

   ### or it can be passed to the function
   if (!is.null(t)) {
      t<-as.data.frame(lapply(t,FUN=as.integer))
      trx<-merge(trx,t,by="time",all.x=T) }

   ### if time protocol information has been passed, calculate quadrant preference
   if ("section" %in% colnames(trx)) {
      preference<-as.integer(trx$distance*trx$quadrant*((trx$section>3)*((trx$section%%2)*2-1)))
      trx<-cbind(trx,preference) }

   ## re-order data in case it's been shuffled by merge
   trx<-trx[order(trx$id,trx$time),]

   ## factorise id
   trx$id<-factor(trx$id)

   if ("trx" %in% names(explist)) {
     explist$trx<-trx
     if (!(("t" %in% names(explist)|(is.null(t))))) {
        explist$t<-t  }
      }
   else { explist<-trx }

   return (explist)
}
