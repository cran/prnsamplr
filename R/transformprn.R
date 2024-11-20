#' Permanent random number transformation
#'
#' @description
#' Transformation of the permanent random numbers used in the sampling
#' procedure, to control the overlap between samples, and thus control the
#' sample coordination. The method used is specified in
#' Lindblom and Teterukovsky (2007).
#'
#' @param frame Data frame (or data.table or tibble) containing the elements
#' to sample from.
#' @param prn Variable in \code{frame} containing the permanent random numbers.
#' @param direction Direction for the enumeration. "U" or "R" for upwards,
#' or equivalently to the right on the real-number line. "D" or "L" for
#' downwards, or equivalently to the left on the real-number line.
#' @param start Starting point for the transformation. For SRS this corresponds
#' to the point at which one wants to start sampling.
#'
#' @return A copy of the input data frame with the permanent random numbers
#' transformed according to specification, along with the numeric variable
#' \code{prn.old} containing the non-transformed permanent random numbers.
#'
#' @export
#'
#' @examples dfOut <- transformprn(
#'   frame = ExampleData,
#'   prn = ~rands,
#'   direction = "U",
#'   start = 0.2
#' )
#' @seealso \link{prnsamplr}, \link{samp}, \link{srs}, \link{pps},
#' \link{ExampleData}
#'
#' @references Lindblom, A. and Teterukovsky, A. (2007). "Coordination of
#' Stratified Pareto pps Samples and Stratified Simple Random Samples at
#' Statistics Sweden." In \emph{Papers presented at the ICES-III, June 18-21,
#' 2007, Montreal, Quebec, Canada.}
transformprn <- function(frame, prn, direction, start) {
  UseMethod("transformprn")
}

#' @export
transformprn.data.frame <- function(frame, prn, direction, start) {
  # start by casting the prn argument as string
  if (is.language(prn)) {
    prn <- rlang::f_name(prn)
  }

  # Check for PRN's on frame
  if (!(prn %in% names(frame))) {
    stop('variable "', prn, '" not found on ', deparse(substitute(frame)))
  }

  # start point needs to be between 0 and 1
  if (start < 0) {
    stop("start point cannot be less than 0")
  }
  if (start > 1) {
    stop("start point cannot be greater than 1")
  }

  # save the original prn's
  frame[["prn.old"]] <- frame[[prn]]
  # transform the prn's according to one operation if counting should go up,
  # another if counting should go down
  # and not at all in any other case
  if (toupper(substr(direction, 1, 1)) %in% c("U", "R")) {
    frame[[prn]] <- (frame[["prn.old"]] - start + 1) %% 1
  } else if (toupper(substr(direction, 1, 1)) %in% c("D", "L")) {
    frame[[prn]] <- 1 - ((frame[["prn.old"]] - start + 1) %% 1)
  } else {
    stop(
      '"', direction,
      '" is not a valid direction. ',
      'Please specify either "U" or "R" for up, or either "D or "L" for down.'
    )
  }
  return(frame)
}

#' @export
transformprn.data.table <- function(frame, prn, direction, start) {
  return(NextMethod() |> data.table::as.data.table())
}

#' @export
transformprn.tbl_df <- function(frame, prn, direction, start) {
  return(NextMethod() |> tibble::as_tibble())
}
