% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distributions.R
\name{pareto_shape}
\alias{pareto_shape}
\title{Calculates the shape parameter for a pareto (type 1) distribution
given the gini coefficient.}
\usage{
pareto_shape(gini)
}
\arguments{
\item{gini}{A number representing the gini coefficient}
}
\value{
the shape
}
\description{
Calculates the shape parameter for a pareto (type 1) distribution
given the gini coefficient.
}
\examples{
pareto_shape(0.5)
pareto_shape(1)
}
