#' @export
srs <- function(df, stratid, nsamp, prn) {
  orderdf <- unname(df[,c(stratid, prn)])
  df <- df[do.call(order, orderdf), ]
  df$sampled <- sequence(rle(as.character(df[, stratid]))$lengths) <= df[, nsamp]
  return(df)
}