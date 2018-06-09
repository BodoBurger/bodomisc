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
#' only head and tail of the data.frame is printed.
#'
#' @param data data.frame
#' @param all.rows=FALSE If TRUE, print out all rows.
#' @param max.rows=15 integer Threshold up to all rows are printed out.
#'
#' @export
#'
#' @examples
#' print(mtcars)
#'
#' print(mtcars, all.rows=TRUE)
print.data.frame = function(data, all.rows=FALSE, max.rows=15) {
  if (nrow(data) <= max.rows | all.rows) {
    base::print.data.frame(data)
  } else {
    capt.print = capture.output(base::print.data.frame(data))
    end = length(capt.print)
    ellipsis = paste(rep("...   ", nchar(capt.print[1])/6), collapse = "")
    # TODO: make ellipsis line look nicer ;)
    cat(capt.print[1:6], sep = "\n")
    cat(ellipsis, "\n")
    cat(capt.print[(end-4):end], sep = "\n")
  }
}
