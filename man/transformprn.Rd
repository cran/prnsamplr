\name{transformprn}
\alias{transformprn}
\title{Permanent random number transformation}

\description{
  Transformation of the permanent random numbers used in the sampling procedure, to control the overlap between samples, and thus control the sample coordination. The method used is specified in Lindblom and Teterukovsky (2007).
}

\usage{
transformprn(df, prn, direction, start)
}

\arguments{
  \item{df}{Data frame containing the elements to sample from.}
  \item{prn}{Variable in \code{df} containing the permanent random numbers.}
  \item{direction}{"U" or "R" for upwards, or to the right on the real-number line. "D" or "L" for downwards, or to the left on the real-number line.}
  \item{start}{Starting point for the transformation.}
}

\value{
  Copy of the input data frame together with the numeric variable \code{prn.old} containing the non-transformed permanent random numbers, as well as the numeric variable \code{prn} containing the transformed permanent random numbers.
}

\examples{
dfOut <- transformprn(df=ExampleData, prn="rands", direction="U", start=0.2)
}

\seealso{
  \code{\link{samp}},
  \code{\link{srs}},
  \code{\link{pps}}.
}

\references{Lindblom, A. and Teterukovsky, A. (2007) \emph{Coordination of Stratified Pareto pps Samples and Stratified Simple Random Samples at Statistics Sweden}.}