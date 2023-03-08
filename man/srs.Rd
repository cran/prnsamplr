\name{srs}
\alias{srs}
\title{Stratified simple random sampling}

\description{
  Stratified simple random sampling (SRS) using permanent random numbers. Can also be used for non-stratified SRS using a dummy stratum taking the same value for each object.
}

\usage{
srs(df, stratid, nsamp, prn)
}

\arguments{
  \item{df}{Data frame containing the elements to sample from.}
  \item{stratid}{Variable in \code{df} containing the strata.}
  \item{nsamp}{Variable in \code{df} containing the sample sizes.}
  \item{prn}{Variable in \code{df} containing the permanent random numbers.}
}

\value{
  Copy of the input data frame together with the boolean variable sampled.
}

\examples{
dfOut <- srs(df=ExampleData, 
             nsamp="nsample", 
             stratid="stratum", 
             prn="rands")
}

\seealso{
  \code{\link{prnsamplr}},
  \code{\link{samp}},
  \code{\link{pps}},
  \code{\link{transformprn}},
  \code{\link{ExampleData}}.
}