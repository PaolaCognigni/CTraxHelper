flydancer <- function (fig) {

  if (mode(fig)=="complex") {
    dx<-(apply(Re(fig),1,diff))
    dy<-(apply(Im(fig),1,diff))
    y<-t(Im(fig))
    setsize<-Re(fig)[,ncol(fig)]/ncol(fig)
  }
  else if (mode(fig)=="list") {
    dx<-apply(fig$x,1,diff)
    dy<-apply(fig$y,1,diff)
    y<-t(fig$y)
    setsize<-fig$x[,ncol(fig$x)]/ncol(fig$x)
  }
  steps<-(dx^2+dy^2)^0.5
  footup<-apply(steps,2,mean)
  dfootup<-apply(steps,2,sd)
  rounds<-apply(y,2,mean)
  drounds<-apply(y,2,sd)
  hey<-apply(abs(y),2,mean)
  dhey<-apply(abs(y),2,sd)
  wriggle<-log(setsize/footup)
  twist<-log(1-abs(rounds)/hey)

  dance<-data.frame(setsize,footup,dfootup,rounds,drounds,hey,dhey,wriggle,twist)
  return(dance)
}
