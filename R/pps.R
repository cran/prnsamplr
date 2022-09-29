#' @export
#' @importFrom stats setNames aggregate
pps <- function(df, stratid, nsamp, prn, size) {
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
