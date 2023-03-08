#' @export
srs <- function(df, stratid, nsamp, prn) {
  # Check if any argument is missing
  if (missing(df)) {
    stop('argument "df" is missing, with no default')
  }
  if (missing(stratid)) {
    stop('argument "stratid" is missing, with no default')
  }
  if (missing(nsamp)) {
    stop('argument "nsamp" is missing, with no default')
  }
  if (missing(prn)) {
    stop('argument "prn" is missing, with no default')
  }
  
  # Check for each variable on df
  if (!(stratid %in% names(df))) {
    stop('variable "', stratid, '" not found on ', deparse(substitute(df)))
  }
  if (!(nsamp %in% names(df))) {
    stop('variable "', nsamp, '" not found on ', deparse(substitute(df)))
  }
  if (!(prn %in% names(df))) {
    stop('variable "', prn, '" not found on ', deparse(substitute(df)))
  }
  
  # nsamp and prn numeric variables
  if (mode(df[, nsamp]) != "numeric") {
    stop('sample size variable ', nsamp, ' is not numeric')
  }
  if (mode(df[, prn]) != "numeric") {
    stop('PRN variable ', prn, ' is not numeric')
  }
  
  # Only one nsamp for each stratid
  StratInfo <- unique(df[, c(stratid, nsamp)])
  n_strat_nsamp <- data.frame(table(StratInfo[, stratid]))
  non_unique_nsamp <- n_strat_nsamp[n_strat_nsamp$Freq > 1,]
  if (nrow(non_unique_nsamp) > 0) {
    problematic_strata <- non_unique_nsamp$Var1
    warning(stratid, ' with names ', paste(problematic_strata, collapse=', '), 
            ' have more than one corresponding value of ', nsamp)
  }
  
  # Each prn between 0 and 1
  prn_below_zero <- df[df[, prn] < 0,]
  if (nrow(prn_below_zero) > 0) {
    warning(prn, ' less than 0 found at rows ', 
            paste(row.names(prn_below_zero), collapse=', '))
  }
  prn_above_one <- df[df[, prn] > 1,]
  if (nrow(prn_above_one) > 0) {
    warning(prn, ' greater than 1 found at rows ', 
            paste(row.names(prn_above_one), collapse=', '))
  }
  
  # sort the frame along stratum and PRN's
  orderdf <- unname(df[,c(stratid, prn)])
  df <- df[do.call(order, orderdf), ]
  # the nsamp first objects in each stratum are marked for sampling
  df$sampled <- sequence(rle(as.character(df[, stratid]))$lengths) <= df[, nsamp]
  return(df)
}