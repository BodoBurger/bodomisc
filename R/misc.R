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
#' x = c(2,3,1,1,1,3,3,2,2,1)
#' v1 = factor(x, levels = c(1, 2, 3), labels = c("red", "blue", "green"))
#' v1
#' FactorMapping(v1)
#' v11 = factor(x, levels = c(3, 2, 1), labels = c("red", "blue", "green"))
#' v11
#' FactorMapping(v11)
#' v111 = factor(x, labels = c("red", "blue", "green"))
#' FactorMapping(v111)
#' v111
#' v1111 = factor(c(4,3,1,1,1,3,3,4,4,1), labels = c("red", "blue", "green"))
#' FactorMapping(v1111)
#' v1111
#' ####################
#' x2 = c(3,3,3,1,1,1,1,5,5,3,1)
#' v2 = ordered(x2, levels = c(1, 3, 5), labels = c("Low", "Medium", "High"))
#' v2
#' ####################
#' test = factor(c("yes","no"))
#' levels(test)
#' test
#' FactorMapping(test)
#'
FactorMapping = function(x) {
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
#' hi("bodomisc")
#' help(package = "bodomisc")
#'
hi = function(package) {
  eval(call("help", package = package))
}


#' Improved print.data.frame
#'
#' If the number of rows of a data.frame exceeds \code{max.rows}
#' only head and tail of the data.frame is printed. Also does not print out all columns
#' if the are more than \code{max.cols}. Function can break if columns contain long strings.
#'
#' @param data data.frame
#' @param all.rows=FALSE If TRUE, print out all rows.
#' @param max.rows=15 integer Threshold for number of rows that are printed out.
#' @param all.cols=FALSE If TRUE, print out all columns.
#' @param max.cols=10 integer Threshold for number of columns that are printed out.
#'
#' @export
#'
#' @examples
#' print(mtcars)
#'
#' print(mtcars, all.rows=TRUE)
print.data.frame = function(data, all.rows=FALSE, max.rows=15, all.cols=FALSE, max.cols=15) {
  rows.ellipsis = FALSE
  cols.ellipsis = FALSE
  n.rows = nrow(data)
  n.cols = ncol(data)
  row.names.not.numeric = any(is.na(suppressWarnings(as.numeric(row.names(data)))))
  if (n.rows <= max.rows | all.rows) rows = 1:n.rows else {
    rows = c(1:5,(n.rows-4):n.rows)
    rows.ellipsis = TRUE
  }
  if (n.cols <= max.cols | all.cols) cols = 1:n.cols else cols = c(1:6,(n.cols-5):n.cols)
  if (is.data.frame(data[rows, cols])) capt.print = capture.output(base::print.data.frame(data[rows, cols]))
  else capt.print = capture.output(base::print(data[rows, cols]))
  if (row.names.not.numeric) {
    row.names.numerical = format(c("# ", paste0(rows, ":")), width = nchar(n.rows),
      justify = "right")
    capt.print = paste(row.names.numerical, capt.print)
  }
  if (rows.ellipsis) capt.print = c(capt.print[1:6], "---", capt.print[7:11])
  cat(capt.print, sep = "\n")
  cat("### data.frame with", n.cols, "columns ###\n")
}
