\name{pps}
\alias{pps}
\title{Stratified probability-proportional-to-size sampling}

\description{
  Stratified probability-proportional-to-size (PPS) sampling using permanent random numbers. Can also be used for non-stratified PPS using a dummy stratum taking the same value for each object.
}

\usage{
pps(df, stratid, nsamp, prn, size)
}

\arguments{
  \item{df}{Data frame containing the elements to sample from.}
  \item{stratid}{Variable in \code{df} containing the strata.}
  \item{nsamp}{Variable in \code{df} containing the sample sizes.}
  \item{prn}{Variable in \code{df} containing the permanent random numbers.}
  \item{size}{Variable in \code{df} containing the size measure.}
}

\value{
  Copy of the input data frame together with the boolean variable \code{sampled}, as well as a numeric variable \code{lambda} containing the estimated first-order inclusion probabilities. 
}

\examples{
dfOut <- pps(df=ExampleData, 
             nsamp="nsample", 
             stratid="stratum", 
             prn="rands", 
             size="sizeM")
}

\seealso{
  \code{\link{samp}},
  \code{\link{srs}},
  \code{\link{transformprn}}.
}