#' Show mapping of factor levels and factor values
#'
#' The way R maps levels to values is often not clear so it can help to print out the explicit
#' mapping.
#'
#' @param x factor variable
#'
#' @return data.frame
#' @export
#'
#' @examples
factorMapping = function(x) {
  if(!is.factor(x)) stop("Object is not a factor.")
  data.frame(levels = unique(x), value = as.numeric(unique(x)))
}
