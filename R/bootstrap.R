bootstrapReplicates = function(model.fun, data, bs.iters, features,
                       at = NULL, predict.fun = NULL, cl = NULL, ...) {

  rdesc = mlr::makeResampleDesc("Bootstrap", iters = bs.iters)
  rin = mlr::makeResampleInstance(rdesc, size = nrow(data))
  ame.replicates = lapply(rin$train.inds, function(i) {
    model = model.fun(formula, data = data[i, ])
    computeAME(model, data[i, ], features = features, at = at,
               predict.fun = predict.fun, cl = cl, ...)
  })
  ame.replicates = do.call(rbind.data.frame, ame.replicates)
  return(ame.replicates)
}

bootstrapEstimate = function(replicates) {

  ame.mean = apply(replicates, MARGIN = 2, FUN = mean)
  ame.sd = apply(replicates, MARGIN = 2, FUN = sd)
  ame.est = list(ame.mean, ame.sd)
  names(ame.est) = c("mean", "sd")
  return(ame.est)
}

bootstrapCI = function(replicates, alpha, method = "percentile") {
  if (method == "percentile") {
    ci = do.call(bootstrapCIPercentile, list(replicates, alpha))
  }
  return(ci)
}

bootstrapCIPercentile = function(replicates, alpha) {
  ci.list = lapply(colnames(replicates), FUN = function(var) {
    reps = replicates[[var]]
    reps.ordered = sort(reps)
    lower.bound = reps.ordered[alpha * length(reps.ordered)]
    upper.bound = reps.ordered[(1 - alpha) * length(reps.ordered)]
    ci = c(lower.bound, upper.bound)
    names(ci) = c(alpha, 1 - alpha)
    return(ci)
  })
  names(ci.list) = colnames(replicates)
  return(ci.list)
}

