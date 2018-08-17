require(mlr)
require(ElemStatLearn)
require(ggplot2)
theme_set(theme_light())

# binary classification, two features
me = ElemStatLearn::mixture.example
df = data.frame(x1 = me$x[,1], x2 = me$x[,2], y = factor(me$y))
tsk = makeClassifTask(data = df, target = "y")
lrn.rf = makeLearner("classif.randomForest")
lrn.knn30 = makeLearner("classif.knn", k = 30)
lrn.knn50 = makeLearner("classif.knn", k = 50)
me.model.rf = train(lrn.rf, tsk)
me.model.knn30 = train(lrn.knn30, tsk)
me.model.knn50 = train(lrn.knn50, tsk)

plotClassification2D(me.model.knn50, tsk, features = c("x1", "x2"),
  x1.lim = c(-8, 8), x2.lim = c(-6, 6), colours = "ESL")

# binary classification, multiple features
spam.rf = train(lrn.rf, spam.task)
spam.knn50 = train(lrn.knn50, spam.task)
spam.knn = train(makeLearner("classif.knn", k = 5), spam.task)

plotClassification2D(spam.knn, spam.task, features = c("charExclamation", "charDollar"),
  x1.lim = c(0,2), x2.lim = c(0,1))

plotClassification2D(spam.knn, spam.task, features = c("you", "charDollar"))


# multiclass classification
iris.knn = train(makeLearner("classif.knn", k = 50), iris.task)
plotClassification2D(iris.knn, iris.task, features = c("Petal.Width", "Petal.Length"))

iris.rf = train(lrn.rf, iris.task)
plotClassification2D(iris.rf, iris.task, features = c("Petal.Width", "Petal.Length"))
plotClassification2D(iris.rf, iris.task, features = c("Sepal.Width", "Sepal.Length"))

# minimal example
library(mlr)
library(ggplot2)
theme_set(theme_light())

# visualize randomForest predictions on iris data
require(randomForest)
iris.mod.rf = train(makeLearner("classif.randomForest"), iris.task)
plotClassification2D(iris.mod.rf, iris.task, features = c("Petal.Length", "Petal.Width"))

# recreate plots from chapter 2 of "Elements of Statistical Learning"
require(ElemStatLearn)
me = ElemStatLearn::mixture.example
df = data.frame(x1 = me$x[,1], x2 = me$x[,2], y = factor(me$y))
tsk = makeClassifTask(data = df, target = "y")
spam.knn = train(makeLearner("classif.knn", k = 15), tsk)
plotClassification2D(spam.knn, tsk, features = c("x1", "x2"), colours = "ESL")

