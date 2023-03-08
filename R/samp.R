#' @export
samp <- function(method, df, ...) {
  # Check if any argument is missing
  if (missing(method)) {
    stop('argument "method" is missing, with no default')
  }
  if (missing(df)) {
    stop('argument "df" is missing, with no default')
  }
  
  # Check if a valid method is used
  method_str <- deparse(substitute(method))
  if (!(method_str %in% c('srs', 'pps'))) {
    stop('invalid method ', method_str)
  }
  
  # run the specified method function using provided parameters
  do.call(method, list(df, ...))
}
