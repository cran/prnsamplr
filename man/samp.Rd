\name{samp}
\alias{samp}
\title{Stratified permanent random number sampling}

\description{
  Wrapper for stratified simple random sampling (SRS) and probability-proportional-to-size (PPS) sampling using permanent random numbers. Can also be used for non-stratified sampling using a dummy stratum taking the same value for each object.
}

\usage{
samp(method, df, ...)
}

\arguments{
  \item{method}{\code{pps} or \code{srs}.}
  \item{df}{Data frame containing the elements to sample from.}
  \item{...}{Further method-specific arguments.}
}

\value{
  Copy of the input data frame together with the boolean variable \code{sampled}, as well as a numeric variable \code{lambda} containing the estimated first-order inclusion probabilities when PPS is used.
}

\examples{
dfOut <- samp(method=pps, 
              df=ExampleData, 
              nsamp="nsample", 
              stratid="stratum", 
              prn="rands", 
              size="sizeM")

dfOut <- samp(method=srs, 
              df=ExampleData, 
              nsamp="nsample", 
              stratid="stratum", 
              prn="rands")
}

\seealso{
  \code{\link{srs}},
  \code{\link{pps}},
  \code{\link{transformprn}}.
}