bootstrapSE = function(model.fun, data, bs.iters,features,
                       at = NULL, predict.fun = NULL, cl = NULL, ...) {

  rdesc = mlr::makeResampleDesc("Bootstrap", iters = bs.iters)
  rin = mlr::makeResampleInstance(rdesc, size = nrow(data))
  ame = lapply(rin$train.inds, function(i) {
    model = model.fun(formula, data = data[i,])
    computeAME(model, data[i,], features = features, at = at,
               predict.fun = predict.fun, cl = cl, ...)
  })
}

bootstrapCI = function(replicates, alpha, method = "percentile") {
  if (method == "percentile") {
    do.call(bootstrapCIPercentile, list(replicates, alpha))
  }
}

bootstrapCIPercentile = function(replicates, alpha) {

  reps.ordered = sort(replicates)
  lower.bound = reps.ordered[alpha * length(reps.ordered)]
  upper.bound = reps.ordered[(1 - alpha) * length(reps.ordered)]
  ci = c(lower.bound, upper.bound)
  names(ci) = c(alpha, 1 - alpha)
  return(ci)
}


