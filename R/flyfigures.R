flyfigures<-function (trx,stepsize=6,output='df') {

  trx<-trx[order(trx$id,trx$time),]
  dx<- trx$x
  dy<- trx$y
  step<-(c(NA,diff(dx))^2+c(NA,diff(dy))^2)^0.5
  dx[step>50]<-NA
  dy[step>50]<-NA
  dxmatrix<-matrix(rep(dx,stepsize),ncol=stepsize)
  dymatrix<-matrix(rep(dy,stepsize),ncol=stepsize)
  idmatrix<-matrix(rep(trx$id,stepsize),ncol=stepsize)

  for (i in 2:stepsize) {
    dxmatrix[,i]<-tail(c(dxmatrix[,i],rep(NA,i-1)),(length(dxmatrix[,i])))
    dymatrix[,i]<-tail(c(dymatrix[,i],rep(NA,i-1)),(length(dymatrix[,i])))
    idmatrix[,i]<-tail(c(idmatrix[,i],rep(NA,i-1)),(length(idmatrix[,i])))
    }

  jumps<-apply(idmatrix,1,FUN=function(a) {length(unique(a))>1})
  dxmatrix[jumps,]<-NA
  dymatrix[jumps,]<-NA

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

##
  if (output=='df') {
     steps<-data.frame('x'=matrix(rotx,ncol=1),'y'=matrix(roty,ncol=1),'step'=rep(1:stepsize,each=length(dx)),'starttime'=rep(trx$time,stepsize),'id'=rep(trx$id,stepsize), 'time'=rep(trx$time,stepsize)+rep(0:(stepsize-1),each=length(dx)))
     steps<-steps[order(steps$id,steps$starttime),]
  }
  else if (output=='complex') {
    steps<-matrix(complex(real=rotx,imaginary=roty),ncol=stepsize)
  }
  else { steps<-list('x'=rotx,'y'=roty, 'id'=idmatrix[,1], 'start'=trx$time)  }

  return(steps)
}


