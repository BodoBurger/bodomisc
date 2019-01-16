#' Visualize a classification task in 2D
#'
#' Plot decision boundaries, prediciton areas and original data for two features.
#'
#' @param model [mlr WrappedModdel object]
#' @param task [mlr classification task object]
#'   Created by mlr::makeClassifTask
#' @param features [\code{character(2)}]
#'   Names of two numeric features.
#' @param grid.res [\code{numeric(1)}]
#' @param x1.lim [\code{numeric(2)}]
#' @param x2.lim [\code{numeric(2)}]
#' @param colours [\code{character(1)}] "ESL": colours from the book "Elements of statistical learning"
#'
#' @return ggplot object
#' @export
#'
#' @examples
#' library(mlr)
#' library(ggplot2)
#' theme_set(theme_light())
#'
#' # visualize randomForest predictions on iris data
#' require(randomForest)
#' iris.mod.rf = train(makeLearner("classif.randomForest"), iris.task)
#' plotClassification2D(iris.mod.rf, iris.task, features = c("Petal.Length", "Petal.Width"))
#'
#' # recreate plots from chapter 2 of "Elements of Statistical Learning"
#' require(ElemStatLearn)
#' me = ElemStatLearn::mixture.example
#' df = data.frame(x1 = me$x[,1], x2 = me$x[,2], y = factor(me$y))
#' tsk = makeClassifTask(data = df, target = "y")
#' spam.knn = train(makeLearner("classif.knn", k = 15), tsk)
#' plotClassification2D(spam.knn, tsk, features = c("x1", "x2"), colours = "ESL")
#'
plotClassification2D = function(model, task, features,
                                grid.res = 100, x1.lim = NULL, x2.lim = NULL,
                                colours = FALSE) {
  data = mlr::getTaskData(task)
  target = mlr::getTaskTargetNames(task)
  x1 = data[, features[1]]
  x2 = data[, features[2]]
  if (is.null(x1.lim)) x1.lim = range(x1)
  if (is.null(x2.lim)) x2.lim = range(x2)
  grid = expand.grid(x1 = seq(x1.lim[1], x1.lim[2], length.out = grid.res),
                     x2 = seq(x2.lim[1], x2.lim[2], length.out = grid.res))
  colnames(grid) = features
  n = nrow(grid)
  other.features = setdiff(colnames(data), c(features, target))
  if (length(other.features) > 0) {
    for (f in other.features) {
      if (is.factor(data[[f]])) {
        grid[f] = majority(data[[f]])
      } else {
        grid[f] = mean(data[,f])
      }
    }
  }
  class = mlr::getPredictionResponse(predict(model, newdata = grid))
  p = ggplot() +
    geom_point(aes(x = grid[, features[1]], y = grid[, features[2]], col = class),
      shape = 20, size = .05, alpha = .5, show.legend = TRUE) +
    geom_point(aes(x = x1, y = x2, col = data[, target]), shape = "o", size = 4, stroke = 2,
               show.legend = TRUE) +
    coord_cartesian(xlim = x1.lim, ylim = x2.lim) + xlab(features[1]) + ylab(features[2])
  n.pred.classes = length(unique(class))
  if (n.pred.classes > 1) {
    p = p + geom_contour(aes(grid[, features[1]], grid[, features[2]], z = as.numeric(class)),
              col = "black", bins = n.pred.classes-1)
    }
  if (colours == "ESL") p = p + scale_colour_manual(values = c("deepskyblue", "orange"))
  return(p)
}
