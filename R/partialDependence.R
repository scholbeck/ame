#' Generate partial dependence data
#'
#' @param model
#' @param data
#' @param target
#' @param feature
#' @param derivative
#' @param uniform
#' @param plot
#'
#' @return
#' @export
#'
#' @examples
partialDependenceData = function(model, data, target, feature, derivative = FALSE, uniform = FALSE, gridsize = 10L, plot = FALSE) {
  # Var 1:
  if (uniform) {
    pd.x.min = min(data[[feature]])
    pd.x.max = max(data[[feature]])
    pd.x = seq(pd.x.min, pd.x.max, length.out = gridsize)
  } else {
    pd.x = data[[feature]]
  }

  # TODO: für die gleichen x-Werte nicht mehrmals Derivative berechnen
  if (derivative) {
    pd.y = unlist(lapply(pd.x, function(i) derivative(x = i, feature = feature, data = data, model = model,
      predict.fun = function(object, newdata) kernlab::predict(object, newdata))))
    # BUG: allgemeine predict function benutzen !!!!!!!
  } else {
    pd.y = unlist(lapply(pd.x, function(i) predictModifiedData(x = i, feature = feature, data = data, model = model,
      predict = function(model, newdata) kernlab::predict(model, newdata))))
    # BUG: allgemeine predict function benutzen !!!!!!!
  }

  if (plot) {
    print(ggplot2::qplot(pd.x, pd.y, xlab = feature, ylab = target) + ggplot2::geom_line())
  }

  return(data.frame(pd.x, pd.y))
}
