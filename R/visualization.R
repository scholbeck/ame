
confintPlot = function(estimate, conf.int) {
  vars = names(estimate$mean)
  ame.estimate = unname(estimate$mean)
  plot.data = data.frame(vars, ame.estimate)
  conf.int = simplify2array(conf.int)
  plot.data$ci.lower = conf.int[1, ]
  plot.data$ci.upper = conf.int[2, ]

  plot = ggplot(data = plot.data) +
    geom_line(
      aes_string(x = "vars", y = "ame.estimate", group = 1)) +
    geom_point(
      aes_string(x = "vars", y = "ame.estimate")) +
    geom_errorbar(
      aes(ymin = ci.lower, ymax = ci.upper, x = vars),
      width = 0.1,
      alpha = 0.6) +
    labs(y = "Bootstrap Estimate for AME", x = "") +
    theme_pubr()
  return(plot)
}

