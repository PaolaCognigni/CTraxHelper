##' ---
##' title: "Script 1.1 - Track garnish"
##' output: html_document
##' ---

#+ include=FALSE
knitr::opts_chunk$set(echo = FALSE)
require(CTraxHelper)

#' Raw track and time protocol data is loaded from **`r getwd()`**.

raw<-readRDS('raw.RDS')
t<-readRDS('t.RDS')

#' The raw data is decorated with:
#'
#' * spatial reference variables (distance from edge, centre, quadrant edges)
#' * frame-by-frame derivatives (speed, yaw, body rotation)
#' * time protocol values (experimental sections and repeats, with incremental timers)
#'
#' For protocol sections **4** and **5**, quadrant preference is computed on the basis of time and location.

garn<-list()
for (i in 1: length(raw)) {
  garn[[i]]<-trxgarnish(raw[[i]],t)
}
names(garn)<-names(raw)
vartype<-lapply(garn[[1]]$trx,class)
rawvars<-colnames(raw[[1]]$trx)
garnvars<-setdiff(names(vartype),rawvars)

#' The data is structured into a `r length(garn)`-element `garn` list that contains:
#'
#' * native variables from CTrax
#'     * categorical: `r names(vartype[rawvars][vartype[rawvars]=="factor"])`
#'     * integer: `r names(vartype[rawvars][vartype[rawvars]=="integer"])`
#'     * numeric: `r names(vartype[rawvars][vartype[rawvars]=="numeric"])`
#'
#' * calculated variables:
#'     * categorical: `r names(vartype[garnvars][vartype[garnvars]=="factor"])`
#'     * integer: `r names(vartype[garnvars][vartype[garnvars]=="integer"])`
#'     * numeric: `r names(vartype[garnvars][vartype[garnvars]=="numeric"])`
#'
#' The list is saved to **`r paste(getwd(),"garn.RDS",sep="/")`**.
#'
saveRDS(garn,file="garn.RDS")

#' `rmarkdown::render('script11.R')`
#'
