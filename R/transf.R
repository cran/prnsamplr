#' @export
transformprn <- function(df, prn, direction, start) {
  df["prn.old"] <- df[prn]
  if (toupper(substr(direction, 1, 1)) %in% c("U", "R")) {
    df[prn] <- (df["prn.old"] - start + 1) %% 1
  }
  else if (toupper(substr(direction, 1, 1)) %in% c("D", "L")) {
    df[prn] <- 1 - ((df["prn.old"] - start + 1) %% 1)
  }
  return(df)
}
