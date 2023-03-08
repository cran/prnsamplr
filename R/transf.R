#' @export
transformprn <- function(df, prn, direction, start) {
  # Check if any argument is missing
  if (missing(df)) {
    stop('argument "df" is missing, with no default')
  }
  if (missing(prn)) {
    stop('argument "prn" is missing, with no default')
  }
  if (missing(direction)) {
    stop('argument "direction" is missing, with no default')
  }
  if (missing(start)) {
    stop('argument "start" is missing, with no default')
  }
  
  # Check for PRN's on df
  if (!(prn %in% names(df))) {
    stop('variable "', prn, '" not found on ', deparse(substitute(df)))
  }
  
  # start point needs to be between 0 and 1
  if (start < 0) {
    stop('start point cannot be less than 0')
  }
  if (start > 1) {
    stop('start point cannot be greater than 1')
  }
  
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
  # and not at all in any other case
  else {
    warning('"', direction, 
            '" is not a valid direction. ',
            'Please specify either "U" or "R" for up, or either "D or "L" for down.')
  }
  return(df)
}
