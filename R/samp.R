#' Stratified permanent random number sampling
#'
#' @description
#' Wrapper for stratified simple random sampling (SRS) and
#' probability-proportional-to-size (PPS) sampling using permanent random
#' numbers. Can also be used for non-stratified sampling using a dummy stratum
#' taking the same value for each object.
#'
#'
#' @param method \code{pps} or \code{srs}.
#' @param frame Data frame (or data.table or tibble) containing the elements
#' to sample from.
#' @param ...
#' Further method-specific arguments.
#'
#' @return
#' A copy of the input data frame together with the boolean variable
#' \code{sampled}, as well as the numeric variables \code{lambda} and \code{Q}
#' when pps is used.
#'
#' @export
#'
#' @examples
#' dfOut <- samp(
#'   method = pps,
#'   frame = ExampleData,
#'   nsamp = ~nsample,
#'   stratid = ~stratum,
#'   prn = ~rands,
#'   size = ~sizeM
#' )
#'
#' dfOut <- samp(
#'   method = srs,
#'   frame = ExampleData,
#'   nsamp = ~nsample,
#'   stratid = ~stratum,
#'   prn = ~rands
#' )
#' @seealso \link{prnsamplr}, \link{srs}, \link{pps}, \link{transformprn},
#' \link{ExampleData}
samp <- function(method, frame, ...) {
  # Check if a valid method is used
  method_str <- deparse(substitute(method))
  if (!(method_str %in% c("srs", "pps"))) {
    stop("invalid method ", method_str)
  }

  # run the specified method function using provided parameters
  do.call(method, list(frame, ...))
}
