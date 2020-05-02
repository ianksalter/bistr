


#' Calculates the standard deviation parameter for a log normal distribution
#' given the gini coefficient.
#'
#' @param gini A number representing the gini coefficient
#' @return the standard deviation
#' @examples
#' lnorm_sdlog(0.5)
#' lnorm_sdlog(1)
lnorm_sdlog <- function(gini){
  erfinv(gini)*2
}

#' Calculates the shape parameter for a pareto (type 1) distribution
#' given the gini coefficient.
#'
#' @param gini A number representing the gini coefficient
#' @return the shape
#' @examples
#' pareto_shape(0.5)
#' pareto_shape(1)
pareto_shape <- function(gini){
  1 / (2 * gini) + 1/2
}


#' Calculates the location parameter for a pareto (type 1) distribution
#' given the mean and gini coefficient.
#'
#' @param mean A number representing the mean
#' @param gini A number representing the gini coefficient
#' @return the shape
#' @examples
#' pareto_shape(1000, 0.5)
#' pareto_shape(1000, 1)
pareto_location <- function(mean, gini){
  shape = pareto_shape(gini)
  ((shape - 1) * mean) / shape
}
