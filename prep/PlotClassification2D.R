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

PlotClassification2D(tsk, me.model.knn50, x1.lim = c(-8, 8), x2.lim = c(-6, 6), colours = "ESL")

# binary classification, multiple features
df = getTaskData(spam.task)

# multiclass classification
df = getTaskData(iris.task)
lrn = makeLearner("classif.")

