#' Visualize a classification task in 2D
#'
#' Plot decision boundaries, prediciton areas and original data for two features.
#'
#' @param task mlr classification task object //
#'             Created by mlr::makeClassifTask
#' @param model mlr WrappedModdel object
#' @param grid.res numeric(1)
#' @param x1.lim numeric(2)
#' @param x2.lim numeric(2)
#' @param colours "ESL": colours from the book "Elements of statistical learning"
#'
#' @return ggplot object
#' @export
#'
#' @examples
plotClassification2D = function(task, model, grid.res = 100,
                                x1.lim = NULL, x2.lim = NULL,
                                colours = FALSE) {
  data = getTaskData(task, target.extra = TRUE)
  x1 = data$data[, 1]
  x2 = data$data[, 2]
  if(is.null(x1.lim)) x1.lim = c(min(x1), max(x1))
  if(is.null(x2.lim)) x2.lim = c(min(x2), max(x2))
  grid = expand.grid(x1 = seq(x1.lim[1], x1.lim[2], length.out = grid.res),
                     x2 = seq(x2.lim[1], x2.lim[2], length.out = grid.res))
  y.hat = getPredictionResponse(predict(model, newdata = grid))
  p = ggplot() +
    geom_point(aes(x = grid$x1, y = grid$x2, col = y.hat), shape = 20, size = .05, alpha = .5,
               show.legend = FALSE) +
    geom_contour(aes(grid$x1, grid$x2, z = as.numeric(y.hat)), col = "black", bins = 1) +
    geom_point(aes(x = x1, y = x2, col = data$target), shape = "o", size = 4, stroke = 2,
               show.legend = FALSE) +
    xlab(names(data$data)[1]) + ylab(names(data$data)[2])
  if(colours == "ESL") p = p + scale_colour_manual(values = c("deepskyblue", "orange"))
  return(p)
}
