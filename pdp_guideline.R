library(mlr)
library(kernlab)
library(ggplot2)

lrn = makeLearner("regr.ksvm")
bh = getTaskData(bh.task)

mod = train(lrn, bh.task)

# Testen:
# Var 1:
partialDependenceData(model = mod$learner.model, data = bh, target = "mdev", feature = "zn",
  uniform = FALSE, plot = TRUE)
# Var 2:
partialDependenceData(model = mod$learner.model, data = bh, target = "mdev", feature = "zn",
  uniform = TRUE, plot = TRUE)
# Var 3:
p = partialDependenceData(model = mod$learner.model, data = bh, target = "mdev", feature = "zn",
  uniform = FALSE, derivative = TRUE, plot = TRUE)
ggplot2::qplot(p$pd.x, p$pd.y) + ggplot2::geom_line()
# Var 4:
partialDependenceData(model = mod$learner.model, data = bh, target = "mdev", feature = "zn",
  uniform = TRUE, derivative = TRUE, plot = TRUE)


#############################################
# 4 verschiedene Varianten implementieren:
#############################################
# 1) same as partial dependence without uniform grid
p = lapply(bh$zn, function(i) predictModifiedData(x = i, feature = "zn", data = bh, model = mod$learner.model,
  predict.fun = function(model, newdata) predict(model, newdata)))
unlist(p)
# mlr - ame branch:
pd = generatePartialDependenceData(mod, bh.task, features = "zn", derivative = FALSE, uniform = FALSE, gridsize = getTaskSize(bh.task))
mean(pd$data$medv)
qplot(bh$zn, unlist(p), xlab = "zn", ylab = "medv") + geom_line()

# 2)
# mlr - ame branch:
pd = generatePartialDependenceData(mod, bh.task, features = "zn", derivative = FALSE, uniform = TRUE, gridsize = 10L)
# verwende gridpunkte statt bh$zn
plotPartialDependence(pd)

# 3)
pd = generatePartialDependenceData(mod, bh.task, features = "zn", derivative = TRUE, uniform = FALSE)
ame.pd = mean(pd$data$medv)
ame = computeAME(mod, data = bh, features = "zn")
pd.deriv = lapply(bh$zn, function(i) derivative(x = i, feature = "zn", data = bh, model = mod$learner.model,
  predict.fun = function(object, newdata) predict(object, newdata)))
mean(unlist(pd.deriv))
# nutze derivative statt predictModifiedData

# 4)
pd = generatePartialDependenceData(mod, bh.task, features = "zn", derivative = TRUE, uniform = TRUE, gridsize = 10L)
mean(pd$data$medv)
# nutze derivative statt predictModifiedData + verwende gridpunkte statt bh$zn

# 1-4) nochmal mit jeweils gridsize <= getTaskSize(bh.task)

# TODO: mit welchen der obigen fälle 3 oder 4 ist AME äquivalent??? Oder auch nicht???



library(mmpf)
fit = mod$learner.model
p = marginalPrediction(bh, "zn", c(nrow(bh), nrow(bh)), fit, uniform = FALSE,
  predict.fun = function(object, newdata) predict(object, newdata = newdata),
  aggregate.fun = function(x) c("mean" = mean(x)))

mean(p$V1.mean)

load_all("../mlr")
pd$data$zn
ame = computeAME(mod, data = bh, features = "zn")
mean(ame$zn)

plotPartialDependence(pd)
