#' @export
transformprn <- function(df, prn, direction, start) {
  # save the original prn's
  df["prn.old"] <- df[prn]
  # transform the prn's according to one operation if counting should go up
  if (toupper(substr(direction, 1, 1)) %in% c("U", "R")) {
    df[prn] <- (df["prn.old"] - start + 1) %% 1
  }
  # and according to another if counting should go down
  else if (toupper(substr(direction, 1, 1)) %in% c("D", "L")) {
    df[prn] <- 1 - ((df["prn.old"] - start + 1) %% 1)
  }
  return(df)
}
