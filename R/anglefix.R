#' Angle difference fixer
#'
#' Resolves discontinuity artefacts when calculating angle differences by always selecting
#' the shortest of the two rotations.
#'
#' Takes an angle (in rad: from -2pi to 2pi; or in degrees: from -360 to 360) and if it's larger than
#' half a circle replaces it with the shortest distance between the same points (in the opposite direction).
#'
#' I don't think the degree version quite works yet.
#'
#' @param a an angle in rad from -2pi (anticlockwise) to +2pi (clockwise)
#' @param deg boolean if you want to enter it in degrees. I don't think it works yet
#'
#' @return
#' An angle that is always between -pi and pi
#'
#' @export
#'
#'
anglefix<-function(a,deg=F) {
  if (deg) {
    a<-a/180*pi
  }
  a<-a-sign(a)*(abs(a)%/%pi)*2*pi
  if (deg) {
    a<-a/pi*180
  }
  return(a)
}
