#' Title
#'
#' @param x
#'
#' @return
#'
#' @examples
prepare = function(x) {

}

#' Scale data vector to 0-1-range (or another selectable range)
#'
#' Alternative names: min-max-scaling, data normalization.
#'
#' For standardazing with sample mean and sample standard deviation use base::scale().
#'
#' See \href{https://en.wikipedia.org/wiki/Feature_scaling}{Feature Scaling Wikipedia},
#' \href{https://en.wikipedia.org/wiki/Normalization_(statistics)}{Normalization Wikipedia}.
#'
#' TODO: support matrices and data.frames
#'
#' @param x numeric
#' @param lower.bound=0 Lowest value after scaling.
#' @param upper.bound=1 Highest value after scaling.
#'
#' @return
#' @export
#'
#' @examples
#' normalize(1:101)
#' normalize(rnorm(10, -10, 10), lower.bound = 0, upper.bound = 100)
#'
normalize = function(x, lower.bound = 0, upper.bound = 1) {
  x.min = min(x)
  c1 = (upper.bound - lower.bound)/(max(x) - x.min)
  c2 = lower.bound - c1 * x.min
  return(c1 * x + c2)
}

#' Put values of vector in bins based on quantiles.
#'
#' @param x (numeric) Vector you want to bin.
#' @param probs (numeric) Default is seq(0, 1, .25) (i.e. quartiles).
#' @param include.lowest=TRUE (logical) Include element with lowest value. Who would not want that
#'   (except the authors of the base cut function)?
#' @param as.factor=FALSE (logical) If TRUE returns a factor with quantile boundaries as level
#'   labels. Level labels can be changed via the labels argument.
#' @param values.as.names=FALSE (logical) Return named vector with values of the input
#'   so that you can see how bin numbers correspond to values of x.
#' @param ... further arguments passed down to the base functions quantile() and cut()
#'
#' @return integer(length(x)) or factor
#' @export
#'
#' @examples
#' quantileBins(1:20, values.as.names = TRUE)
#'
#' n = 200
#' x = runif(n, min = 0, max = 1)
#' x1 = x + rnorm(n, 0, 0.05)
#' x2 = x + rnorm(n, 0, 0.05)
#' y = x1 + x2^2 + rnorm(n, 0, 0.1)
#' plot(x1, x2, cex = quantileBins(y, probs = seq(0,1,.1))/5,
#'   col = quantileBins(y))
quantileBins = function(x, probs = c(0, .25, .5, .75, 1), include.lowest = TRUE, as.factor = FALSE,
    values.as.names = FALSE, ...) {
  assertNumeric(x)
  assertNumeric(probs)
  assertLogical(include.lowest)
  assertLogical(as.factor)
  assertLogical(values.as.names)
  bins = cut(x, quantile(x, probs = probs, ...), include.lowest = include.lowest, ...)
  if (as.factor) return(bins)
  else {
    if (values.as.names) return(setNames(as.integer(bins), x)) else return(as.integer(bins))
  }
}
