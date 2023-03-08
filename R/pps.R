#' @export
#' @importFrom stats setNames aggregate
pps <- function(df, stratid, nsamp, prn, size) {
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
  if (missing(size)) {
    stop('argument "size" is missing, with no default')
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
  if (!(size %in% names(df))) {
    stop('variable "', size, '" not found on ', deparse(substitute(df)))
  }
  
  # nsamp, prn, and size numeric variables
  if (mode(df[, nsamp]) != "numeric") {
    stop('sample size variable ', nsamp, ' is not numeric')
  }
  if (mode(df[, prn]) != "numeric") {
    stop('PRN variable ', prn, ' is not numeric')
  }
  if (mode(df[, size]) != "numeric") {
    stop('size variable ', size, ' is not numeric')
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
  
  # calculate the sum of the size in each stratum
  sumdf <- setNames(aggregate(df[size], df[stratid], sum), c(stratid, "sumsize"))
  df <- merge(df, sumdf, by = stratid)
  # calculate a preliminary lambda for each item
  df["lambda"] <- df[nsamp] * df[size] / df["sumsize"]

  # if any lambda >= 1:
  if (any(df["lambda"] >= 1)) {
    # calculate new sample size among units with lambda < 1
    ndf <- aggregate(list(ntot = df["lambda"] >= 1), df[stratid], sum)
    df["nnew"] <- df[nsamp] - merge(df, ndf, by = stratid)["lambda.y"]
    # remove the variable with sum of size
    if ("sumsize" %in% colnames(df)) {
      df["sumsize"] <- NULL
    }
    # extract units with lambda >= 1, set their lambda to 1 and mark them for sampling
    gtone <- subset(df, df$lambda >= 1)
    gtone$lambda <- rep(1, nrow(gtone))
    gtone$Q <- rep(0, nrow(gtone))
    gtone$sampled <- rep(TRUE, nrow(gtone))
    # remove the variable with new sample size from the {lambda >= 1} subset
    gtone["nnew"] <- NULL
    # run the entire function on the {lambda < 1} subset with the new sample sizes
    ltone <- pps(subset(df, df$lambda < 1), stratid, "nnew", prn, size)
    # remove the new sample sizes and return a concatenation of the subsets
    ltone["nnew"] <- NULL
    dfout <- rbind(gtone, ltone)
    return(dfout)
  # if no lambda >= 1
  } else {
    # calculate Q and sort along it for each stratum
    df["Q"] <- df[prn] * (1 - df["lambda"]) / (df["lambda"] * (1 - df[prn]))
    orderdf <- unname(df[,c(stratid, "Q")])
    df <- df[do.call(order, orderdf),]
    # the nsamp with lowest Q for each stratum are marked for sampling
    df$sampled <- sequence(rle(as.character(df[, stratid]))$lengths) <= df[nsamp]
    # remove the sum of the sizes and return the frame
    df["sumsize"] <- NULL
    return(df)
  }
}
