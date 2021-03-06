% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trxfriends.R
\name{trxfriends}
\alias{trxfriends}
\title{Track social interaction annotator}
\usage{
trxfriends(trx)
}
\arguments{
\item{trx}{a track data frame}
}
\value{
A data frame containing, for each fly (\code{id}) and timepoint (\code{time}),
the distance (\code{closest}) and identity (\code{bff}) of the closest conspecific.
}
\description{
Calculates closest other fly for each fly and timepoint. Currently obscenely slow.
}
\details{
The function loops through each frame, calculates an (n x n-1) distance matrix and looks for value and index of minima.
There may be slightly more efficient ways of doing this.
}

