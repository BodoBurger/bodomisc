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

#' Show package documentation index (help index)
#'
#' This is just a shorter way of calling \code{help(package = "package name")}.
#' If you use RStudio you do not have to type the quotation marks (""),
#' just autocomplete the package name by pressing TAB.
#'
#' @param package name of the package
#'
#' @export
#'
#' @examples
hi = function(package) {
  eval(call("help", package = package))
}
