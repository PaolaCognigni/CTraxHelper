#' Time protocol writer
#'
#' Creates and collates time protocol data given minimal information (light pattern and protocol steps)
#'
#' Protocol steps are assumed to follow the scheme:
#'
#' \itemize{
#'
#' \item{0}{: no video (disregarded)}
#' \item{1}{: no light (disregarded)}
#' \item{2}{: lights constantly on}
#' \item{3}{: lights on in a blinking pattern}
#' \item{4}{: quadrants on, NE and SW}
#' \item{5}{: quadrants on, NW and SE}
#'}
#'
#' This is primarily for the collapse of 4 and 5 into a single chunk.
#'
#' There is currently no data cleanup. The underlying noise therefore shows up in frame variability (~4 frame jitter).
#'
#' @param lit a boolean or 0-1 integer vector indicating frames with lights on or off
#' @param prot a list of protocol steps, in order; it can contain 0's and 1's, but only 2-5 are actually used
#' @param sec3 an optional section 3 add-on to stick the last 2s back onto the protocol (input in frames!!!)
#'
#' @return
#'
#' A data frame containing the columns \code{time} (frame number), \code{lit} (lights on or off, 0-1 integer),
#' \code{chunk} (broad section definer including on frames and surrounding off frames), \code{section} (narrow section
#' definer attributing values to each on and off time separately), \code{rept} (repeat: used in conjunction with \code{section}
#' to uniquely identify each lights-on event) and various timers centered at the appropriate light-on time (\code{chunktimer},
#' \code{sectiontimer}, and \code{lighttimer}).
#'
#' @export
#'
#'
timewriter <- function (lit,prot,sec3=0) {
  # this is just to make sure lit ranges form 0 to 1; it should accept either logicals or integers
  lit<-as.integer(as.logical(lit))
  lightson<-which(diff(lit)>0)+1
  duration<-length(lit)

  # simplificaton step: you can give it just the sequence of light activation patterns, ignoring off periods.
  # this calculatin is just to allow it to take the Matlab driver sequence as acceptable input
  patterns<-prot[prot>1]

  # sectionlit uses morphological closing to merge the bursts of protocol section 2 into one event
  # note that this event is still a bit shorter than a 30s protocol because the 2s of light off at the
  # end of the last repeat get incorporated into the following rest
  # Not sure if this is something I can be arsed to fix
  sectionlit<-mmand::closing(lit,rep(1,51))

  # times of light on and off are picked from changes in sectionlit, +1 because diff staggers by half and falls
  # into the previous frame. From this the median section size can be calculated (probably slightly underestimated
  # because the section 2 lengths are trimmed by 2s)
  section.on<-which(diff(sectionlit)>0)+1
  section.off<-which(diff(sectionlit)<0)+1

  # optional section 3 correction: the light-based protocol detection places the last 2s of the 30s of protocol 3
  # into the following dark section. This is a manual fudge to tack them back on.
  sectionlit[section.off[patterns==3]+sec3]<-1
  sectionlit<-mmand::closing(sectionlit,rep(1,51))

  section.off[(patterns==3)]<-section.off[(patterns==3)]+(sec3)

  sectionsize<-round(median(section.off-section.on))
  # IMPOSE option: use factual protocol info to say that a section is actually 18 fps x 30s = 540?

  # chunkswitch: select points at midpoint b/ween off and on to indicate you've moved to a new part
  # of the protocol. You need to guess two additional points before the beginning and after the end
  # of the whole protocol. These are estimated adding or subtracting the presumed section size.
  chunkswitch<-(c(section.on,sectionsize+tail(section.on,1))+c(head(section.off,1)-sectionsize,section.off))%/%2


  if (length(patterns)!=length(section.on)) {
    stop ("I need a protocol identity for each light event! I have ",length(section.on)," light events and ",length(patterns)," event ids")
  }

  # initialise some variables to fill out in the for loop
  # it's going to be for looping all the way! Who cares about inefficiency?
  time<-1:duration
  chunk<-rep(0, duration)
  section<-rep(0, duration)
  chunktimer<-integer(duration)
  sectiontimer<-integer(duration)
  lighttimer<-integer(duration)
  rept<-integer(duration)

  for (t in head(chunkswitch,1):tail(chunkswitch,1)) {
    whichstep<-max(tail(which(chunkswitch<=t),1),0)
    if (whichstep!=0) {

      section[t]<-sectionlit[t]*patterns[whichstep]
      chunk[t]<-patterns[whichstep]
      chunktimer[t]<-t-section.on[whichstep]

      whichend<-whichstep-(t<section.on[whichstep])
      sectiontimer[t]<-(t-section.on[whichstep])*sectionlit[t]+(t-section.off[whichend])*(1-sectionlit[t])

      whichlight<-which.min(abs(t-lightson))
      lighttimer[t]<-t-lightson[whichlight]
      rept[t]<-sum(patterns[1:whichstep]==patterns[whichstep])
    }
  }
  lighttimer[1:section.on[1]-1]<-(-section.on[1]+1):(-1)
  chunk[chunk==5]<-4
  t<-data.frame(time,lit,chunk,section,chunktimer,sectiontimer,lighttimer,rept)
  return(t)
}
