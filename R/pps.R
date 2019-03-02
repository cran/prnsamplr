pps <- function(df, stratid, nsamp, prn, size) {
  sumdf <- setNames(aggregate(df[size], df[stratid], sum), c(stratid, "sumsize"))
  df <- merge(df, sumdf, by = stratid)
  df["lambda"] <- df[nsamp] * df[size] / df["sumsize"]

  if (any(df["lambda"] >= 1)) {
    df$lambda[df$lambda >= 1] <- 1
    ndf <- aggregate(list(ntot = df["lambda"] >= 1), df[stratid], sum)
    df["nnew"] <- df[nsamp] - merge(df, ndf, by = stratid)["lambda.y"]
    if ("sumsize" %in% colnames(df)) {
      df["sumsize"] <- NULL
    }
    gtone <- subset(df, df$lambda >= 1)
    gtone$Q <- rep(NA, nrow(gtone))
    gtone$sampled <- rep(TRUE, nrow(gtone))
    gtone["nnew"] <- NULL
    ltone <- pps(subset(df, df$lambda < 1), stratid, "nnew", prn, size)
    ltone["nnew"] <- NULL
    dfout <- rbind(gtone, ltone)
    return(dfout)
  }

  else {
    df["Q"] <- df[prn] * (1 - df["lambda"]) / (df["lambda"] * (1 - df[prn]))
    df <- df[with(df, order(df[stratid], df["Q"])), ]
    df$sampled <- sequence(rle(as.character(df[, stratid]))$lengths) <= df[nsamp]
    df["sumsize"] <- NULL
    return(df)
  }
}
