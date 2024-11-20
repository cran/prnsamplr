#' Stratified probability-proportional-to-size sampling
#'
#' @description
#' Stratified probability-proportional-to-size (Pareto PiPS) sampling using
#' permanent random numbers. Can also be used for non-stratified Pareto PiPS
#' using a dummy stratum taking the same value for each object.
#'
#' @param frame Data frame (or data.table or tibble)
#' containing the elements to sample from.
#' @param stratid Variable in \code{frame} containing the strata.
#' @param nsamp Variable in \code{frame} containing the sample sizes.
#' @param prn Variable in \code{frame} containing the permanent random numbers.
#' @param size Variable in \code{frame} containing the size measure.
#'
#' @return
#' A copy of the input sampling frame together with the boolean variable
#' \code{sampled}, indicating sample inclusion, as well as a numeric variable
#' \code{lambda} containing the estimated first-order inclusion probabilities
#' and the numeric variable \deqn{Q = \frac{prn(1 - lambda)}{lambda(1 - prn)}}
#' that determines which elements are sampled.
#'
#' @export
#'
#' @examples dfOut <- pps(
#'   frame = ExampleData,
#'   nsamp = ~nsample,
#'   stratid = ~stratum,
#'   prn = ~rands,
#'   size = ~sizeM
#' )
#' @seealso \link{prnsamplr}, \link{samp}, \link{srs}, \link{transformprn},
#' \link{ExampleData}
pps <- function(frame, stratid, nsamp, prn, size) {
  UseMethod("pps")
}

#' @export
pps.data.frame <- function(frame, stratid, nsamp, prn, size) {
  # start by casting arguments as strings
  if (is.language(stratid)) {
    stratid <- rlang::f_name(stratid)
  }
  if (is.language(nsamp)) {
    nsamp <- rlang::f_name(nsamp)
  }
  if (is.language(prn)) {
    prn <- rlang::f_name(prn)
  }
  if (is.language(size)) {
    size <- rlang::f_name(size)
  }

  # Check for each variable on frame
  if (!(stratid %in% names(frame))) {
    stop('variable "', stratid, '" not found on ', deparse(substitute(frame)))
  }
  if (!(nsamp %in% names(frame))) {
    stop('variable "', nsamp, '" not found on ', deparse(substitute(frame)))
  }
  if (!(prn %in% names(frame))) {
    stop('variable "', prn, '" not found on ', deparse(substitute(frame)))
  }
  if (!(size %in% names(frame))) {
    stop('variable "', size, '" not found on ', deparse(substitute(frame)))
  }

  # nsamp, prn, and size numeric variables
  if (!is.numeric(frame[[nsamp]])) {
    stop("sample size variable ", nsamp, " is not numeric")
  }
  if (!is.numeric(frame[[prn]])) {
    stop("PRN variable ", prn, " is not numeric")
  }
  if (!is.numeric(frame[[size]])) {
    stop("size variable ", size, " is not numeric")
  }

  # Only one nsamp for each stratid
  strat_info <- unique(frame[, c(stratid, nsamp)])
  n_strat_nsamp <- data.frame(table(strat_info[, stratid]))
  non_unique_nsamp <- n_strat_nsamp[n_strat_nsamp$Freq > 1, ]
  if (nrow(non_unique_nsamp) > 0) {
    problematic_strata <- non_unique_nsamp$Var1
    warning(
      stratid, " with names ", paste(problematic_strata, collapse = ", "),
      " have more than one corresponding value of ", nsamp
    )
  }

  # Each prn between 0 and 1
  prn_below_zero <- frame[frame[, prn] < 0, ]
  if (nrow(prn_below_zero) > 0) {
    warning(
      prn, " less than 0 found at rows ",
      paste(row.names(prn_below_zero), collapse = ", ")
    )
  }
  prn_above_one <- frame[frame[, prn] > 1, ]
  if (nrow(prn_above_one) > 0) {
    warning(
      prn, " greater than 1 found at rows ",
      paste(row.names(prn_above_one), collapse = ", ")
    )
  }

  # calculate the sum of the size in each stratum
  sum_frame <- setNames(
    aggregate(frame[size], frame[stratid], sum),
    c(stratid, "sumsize")
  )
  frame <- merge(frame, sum_frame, by = stratid)
  # calculate a preliminary lambda for each item
  frame[["lambda"]] <- frame[[nsamp]] * frame[[size]] / frame[["sumsize"]]

  # if any lambda >= 1:
  if (any(frame[["lambda"]] >= 1)) {
    # calculate new sample size among units with lambda < 1
    n_frame <- aggregate(
      list(ntot = frame["lambda"] >= 1),
      frame[stratid], sum
    )
    frame[["nnew"]] <- frame[[nsamp]] - merge(frame,
      n_frame,
      by = stratid
    )[["lambda.y"]]
    # remove the variable with sum of size
    if ("sumsize" %in% colnames(frame)) {
      frame[["sumsize"]] <- NULL
    }
    # extract units with lambda >= 1, set their lambda to 1
    # and mark them for sampling
    gtone <- subset(frame, frame$lambda >= 1)
    gtone$lambda <- rep(1, nrow(gtone))
    gtone$Q <- rep(0, nrow(gtone))
    gtone$sampled <- rep(TRUE, nrow(gtone))
    # remove the variable with new sample size from the {lambda >= 1} subset
    gtone[["nnew"]] <- NULL
    # run the entire function on the {lambda < 1} subset
    # with the new sample sizes
    ltone <- pps(subset(frame, frame$lambda < 1), stratid, ~nnew, prn, size)
    # remove the new sample sizes and return a concatenation of the subsets
    ltone[["nnew"]] <- NULL
    out_frame <- rbind(gtone, ltone)
    return(out_frame)
    # if no lambda >= 1
  } else {
    # calculate Q and sort along it for each stratum
    frame[["Q"]] <- frame[[prn]] * (1 - frame[["lambda"]]) /
      (frame[["lambda"]] * (1 - frame[[prn]]))
    order_frame <- unname(frame[, c(stratid, "Q")])
    out_frame <- frame[do.call(order, order_frame), ]
    # the nsamp with lowest Q for each stratum are marked for sampling
    out_frame$sampled <-
      sequence(rle(as.character(frame[, stratid]))$lengths) <=
        out_frame[nsamp]
    # remove the sum of the sizes and return the frame
    out_frame[["sumsize"]] <- NULL
    return(out_frame)
  }
}

#' @export
pps.data.table <- function(frame, stratid, nsamp, prn, size) {
  return(NextMethod() |> data.table::as.data.table())
}

#' @export
pps.tbl_df <- function(frame, stratid, nsamp, prn, size) {
  return(NextMethod() |> tibble::as_tibble())
}
