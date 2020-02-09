#' @export
srs <- function(df, stratid, nsamp, prn) {
  df <- df[with(df, order(get(stratid), get(prn))), ]
  df$sampled <- sequence(rle(as.character(df[, stratid]))$lengths) <= df[, nsamp]
  return(df)
}