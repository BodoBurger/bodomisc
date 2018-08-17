## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  fig.width = 7, fig.height = 5,
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
library(bodomisc)
library(mlr)
library(ggplot2)
theme_set(theme_light())
library(ElemStatLearn)
me = ElemStatLearn::mixture.example
df = data.frame(x1 = me$x[,1], x2 = me$x[,2], y = factor(me$y))
tsk = makeClassifTask(data = df, target = "y")
spam.knn = train(makeLearner("classif.knn", k = 15), tsk)
plotClassification2D(spam.knn, tsk, features = c("x1", "x2"), colours = "ESL")

## ------------------------------------------------------------------------
iris.mod.rf = train(makeLearner("classif.randomForest"), iris.task)
plotClassification2D(iris.mod.rf, iris.task, features = c("Sepal.Length", "Sepal.Width"))
plotClassification2D(iris.mod.rf, iris.task, features = c("Petal.Length", "Petal.Width"))

## ---- fig.show='hold'----------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(head(mtcars, 10))

