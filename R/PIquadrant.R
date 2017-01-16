#' Preference Index calculator
#'
#' Calculates preference indices for alternating quadrant sections of optogenetic protocol (section 4 and 5).
#'
#' Frankly I can't remember what happens in this function and that is why you should document
#' this shit when you write it. I think it picks out the last 5s of each quadrant section and averages the distribution for each fly
#' (\code{by.fly=T}) or each repeat of the protocol (\code{by.fly=F}). However I see a lot of weird \code{NA}-based analysis in the code
#' and I can't remember what it's for.
#'
#' @param trx input track data frame (must include \code{preference}, \code{sectiontimer} and \code{rept}).
#' Realistically it also requires the usual \code{trx} columns (in particular \code{id})
#' @param by.fly boolean to decide whether you calculate PI by section (of the 4 in the standard protocol,
#' arranged by section id x repeat)
#' or by fly id (collating all sections together for each fly)
#'
#' @return A data frame with the PI for each fly (if \code{by.fly} is true) or by section and repeat
#' (if \code{by.fly} is false).
#' @export
#
#'
PIquadrant <- function(trx, by.fly=F) {
   if (any(!c("preference","sectiontimer","rept")%in% colnames(trx))) {
     print ("Insufficient data: preference, repeat and section info required") }
   else {
     if (by.fly==T) {
        a<-trx[trx$section%in%4:5,]
        timend<-max(a$sectiontimer)
        a<-a[a$sectiontimer>=(timend-90),]
        sides<-with(a,aggregate(preference>0,by=list(id),FUN=sum,na.rm=T))
        totals<-with(a,aggregate(!is.na(preference),by=list(id),FUN=sum,na.rm=T))
        PI5s<-data.frame(sides[,1],(2*sides$x-totals$x)/totals$x)
        colnames(PI5s)[1:2]<-c("id","PI")
        return(PI5s) }
     else {
        a<-trx[trx$section%in%4:5,]
        timend<-max(a$sectiontimer)
        a<-a[a$sectiontimer>=(timend-90),]
        sides<-with(a,aggregate(preference>0,by=list(section,rept),FUN=sum,na.rm=T))
        totals<-with(a,aggregate(!is.na(preference),by=list(section,rept),FUN=sum,na.rm=T))
        PI5s<-cbind(sides[,1:2],(2*sides$x-totals$x)/totals$x)
        colnames(PI5s)[1:3]<-c("section","rept","PI")
        return(PI5s) }
   }
}
