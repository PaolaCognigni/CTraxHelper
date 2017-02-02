#' Track segmentation and realignment
#'
#' Aligns \code{stepsize} consecutive frames from each start to a reference coordinate system where start is
#' always (0,0) and end is always (x,0).
#'
#' From each frame, \code{stepsize} frames are selected and the position data (\code{x} and \code{y})
#' are aligned (shifted and rotated) to a common reference system, yielding transformed \code{x} and \code{y}.
#' These are collected into a matrix of \code{frame} by \code{stepsize} elements (thus oversampling the data).
#'
#' @param trx a track data frame
#' @param stepsize the size of the time window (in frames)
#' @param output whether to output the minimal data (a matrix containing the x,y data as the real and imaginary
#' parts of a complex number) or a more intuitive but somwehat fiddlier list format.
#'
#' @return either a \code{frame} by \code{stepsize} complex matrix (if \code{output='complex'}) where Re is
#' x and Im is y, or a list containing a matrix for \code{x}, one for \code{y}, and vectors for \code{id} and
#' \code{starttime} to match each matrix row to a frame and fly unambiguosly.
#'
#' @export
#'
#'
flyfigures<-function (trx,stepsize=6,output='list') {

  trx<-trx[order(trx$id,trx$time),]
  dx<- trx$x
  dy<- trx$y

  step<-(c(NA,diff(dx))^2+c(NA,diff(dy))^2)^0.5
  dx[step>50]<-NA
  dy[step>50]<-NA
  dxmatrix<-matrix(rep(dx,stepsize),ncol=stepsize)
  dymatrix<-matrix(rep(dy,stepsize),ncol=stepsize)
  idmatrix<-matrix(rep(trx$id,stepsize),ncol=stepsize)

  if ("spin" %in% colnames(trx)) {
    spin<-trx$spin
    spin[step>50]<-NA
    spinmatrix<-matrix(rep(spin,stepsize),ncol=stepsize)
    }

  for (i in 2:stepsize) {
    dxmatrix[,i]<-tail(c(dxmatrix[,i],rep(NA,i-1)),(length(dxmatrix[,i])))
    dymatrix[,i]<-tail(c(dymatrix[,i],rep(NA,i-1)),(length(dymatrix[,i])))
    idmatrix[,i]<-tail(c(idmatrix[,i],rep(NA,i-1)),(length(idmatrix[,i])))
    if ("spin" %in% colnames(trx)) {
      spinmatrix[,i]<-tail(c(spinmatrix[,i],rep(NA,i-1)),(length(spinmatrix[,i]))) }
    }

  jumps<-apply(idmatrix,1,FUN=function(a) {length(unique(a))>1})
  dxmatrix[jumps,]<-NA
  dymatrix[jumps,]<-NA
  if ("spin" %in% colnames(trx)) {
    spinmatrix[jumps,]<-NA
    spinmatrix[!complete.cases(spinmatrix),]<-NA
    }

  # zero shift
  dxmatrix<-dxmatrix-dxmatrix[,1]
  dymatrix<-dymatrix-dymatrix[,1]

  # rotation
  denom<-((dxmatrix[,stepsize])^2+(dymatrix[,stepsize])^2)^0.5
  rotx<-(sweep(dxmatrix,1,dxmatrix[,stepsize],'*') + sweep(dymatrix,1,dymatrix[,stepsize],'*'))
  roty<-(sweep(dymatrix,1,dxmatrix[,stepsize],'*') - sweep(dxmatrix,1,dymatrix[,stepsize],'*'))

  rotx<-(rotx/denom)
  roty<-(roty/denom)

  rotx[!complete.cases(rotx),]<-NA
  roty[!complete.cases(roty),]<-NA

  if (output=='complex') {
    steps<-matrix(complex(real=rotx,imaginary=roty),ncol=stepsize)
  }

  else { steps<-list('x'=rotx,'y'=roty, 'id'=idmatrix[,1], 'start'=trx$time)
    if ("spin" %in% colnames(trx)) { steps$spin<-spinmatrix }
  }

  return(steps)
}


