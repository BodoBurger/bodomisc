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

#' Return most frequent level of factor (majority voting)
#'
#' @section TODO:
#'   If the majority voting is ambiguous: random choice or preferred level.
#'
#' @param x a factor
#'
#' @return factor of length 1 with the same levels as x
#' @export
#'
#' @examples
#' x1 = c(2,3,1,1,1,3,3,2,2,1)
#' f1 = factor(x1, levels = c(1, 2, 3), labels = c("red", "blue", "green"))
#' f1
#' majority(f1)
#'
#' x2 = c(2,3,1,1,1,3,3,2,2,1,2)
#' f2 = factor(x2, levels = c(1, 2, 3), labels = c("red", "blue", "green"))
#' f2
#' majority(f2)
#'
#' x3 = c(3,3,3,1,1,1,1,5,5,3,1)
#' o3 = ordered(x2, levels = c(1, 3, 5), labels = c("Low", "Medium", "High"))
#' o3
#' majority(o3)
majority = function(x) {
  if (!is.factor(x)) stop("x is not a factor.")
  level = names(which.max(table(x)))
  x[1:length(x)] = level
  return(x[1])
}

#' Improved print.data.frame
#'
#' This function improves readability of printing data.frame (avoids filling your console with clutter).
#' Also displays number of rows and columns.
#'
#' If the number of rows of a data.frame exceeds \code{max.rows}
#' only head and tail of the data.frame is printed. Also does not print out all columns
#' if there are more than \code{max.cols}. Function can break if columns contain long strings.
#'
#' @param x [\code{data.frame}]
#' @param all.rows [\code{logical}] If TRUE, print out all rows.
#' @param max.rows [\code{integer}] Threshold for number of rows that are printed out.
#' @param all.cols [\code{logical}] If TRUE, print out all columns.
#' @param max.cols [\code{integer}] Threshold for number of columns that are printed out.
#' @param ... optional arguments of \code{base::print.data.frame}
#'
#' @export
#'
#' @examples
#' print(mtcars)
#'
#' print(mtcars, all.rows=TRUE)
print.data.frame = function(x, ..., all.rows=FALSE, max.rows=30, all.cols=FALSE, max.cols=15) {
  rows.ellipsis = FALSE
  n.rows = nrow(x)
  n.cols = ncol(x)
  row.names.not.numeric = any(is.na(suppressWarnings(as.numeric(row.names(x)))))
  if (n.rows <= max.rows | all.rows) rows = 1:n.rows else {
    rows = c(1:5,(n.rows-4):n.rows)
    rows.ellipsis = TRUE
  }
  if (n.cols <= max.cols | all.cols) cols = 1:n.cols else cols = c(1:6,(n.cols-5):n.cols)
  capt.print = capture.output(base::print.data.frame(x[rows, cols, drop=FALSE], ...))
  if (rows.ellipsis) capt.print = c(capt.print[1:6], "---", capt.print[7:11])
  if (row.names.not.numeric) {
    rnn = format(c("# ", paste0(rows, ":")), width = nchar(n.rows), justify = "right")
    if (rows.ellipsis) rnn = c(rnn[1:6], substring("          ", 1, nchar(rnn[1])), rnn[7:11])
    capt.print = paste(rnn, capt.print)
  }
  cat(capt.print, sep = "\n")
  cat("### data.frame with", n.rows, "rows and", n.cols, "columns ###\n")
}
