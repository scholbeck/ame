% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/derivative.R
\name{derivative}
\alias{derivative}
\title{Computes the derivative of a feature}
\usage{
derivative(x, feature, data, model, predict.fun = function(object, newdata)
  predict(object, newdata = newdata), ...)
}
\arguments{
\item{x}{[\code{vector}]\cr
A scalar value or vector indicating the point(s) at which the gradient is to be calculated.}

\item{feature}{[\code{character(1)}]\cr
The column name of the data set that refers to the feature for which the derivative will be computed.}

\item{data}{[\code{logical(1)}]\cr
The data set that was used to fit the model.}

\item{model}{[\code{\link[mlr]{WrappedModel}} | \code{any}]\cr
A model object. Can be also an object of class \code{\link[mlr]{WrappedModel}}.}

\item{predict.fun}{[\code{function(1)}]\cr
The function that should be used to generate predictions from \code{model}.
This function must have two arguments, \code{object} and \code{newdata}
The default is the predict method for \code{model}.
If \code{model} is of class \code{\link[mlr]{WrappedModel}}, the default tries to use
\code{\link[mlr]{getPredictionProbabilities}} or \code{\link[mlr]{getPredictionResponse}}
depending on whether \code{model} is a classification or regression problem.}

\item{...}{Further options passed down to the \code{\link[numDeriv]{grad}} function.}
}
\description{
Computes the derivative of a feature at point or vector x.
}
