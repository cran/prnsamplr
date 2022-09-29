#' @export
samp <- function(method, df, ...) {
  # run the specified method function using provided parameters
  do.call(method, list(df, ...))
}
