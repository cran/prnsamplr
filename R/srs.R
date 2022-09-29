#' @export
srs <- function(df, stratid, nsamp, prn) {
  # sort the frame along stratum and PRN's
  orderdf <- unname(df[,c(stratid, prn)])
  df <- df[do.call(order, orderdf), ]
  # the nsamp first objects in each stratum are marked for sampling
  df$sampled <- sequence(rle(as.character(df[, stratid]))$lengths) <= df[, nsamp]
  return(df)
}